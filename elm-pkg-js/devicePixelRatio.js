exports.init = async function init(app)
{
    app.ports.set_overscroll.subscribe(bool => {
        console.log("overscroll" + bool);
        if (bool) {
            document.body.style = "overscroll-behavior: auto;";
        }
        else
        {
            document.body.style = "overscroll-behavior: none;"
        }
    });

    var existingHandle = null;

    app.ports.get_file_api_not_supported.subscribe((a) => {
        if (!(window.showOpenFilePicker && window.showSaveFilePicker)) {
            // For some reason Elm doesn't pick up on this port call unless there's a delay
            setTimeout(function () {
                app.ports.got_file_api_not_supported.send(null);
            }, 1000);
        }
    });

    app.ports.select_file_to_js.subscribe((a) => {
        let options = {
            types: [
              {
                description: "End-to-end test module",
                accept: {
                  "text/*": [".elm"],
                },
              },
            ],
            excludeAcceptAllOption: true,
            multiple: false,
          };
        if (window.showOpenFilePicker) {
            window.showOpenFilePicker(options).then((result) => {
                existingHandle = result[0];
                existingHandle.getFile().then((file) => {
                    let reader = new FileReader();
                    reader.readAsText(file);
                    reader.onload = function() {
                        app.ports.select_file_from_js.send({ name : file.name, content : reader.result });
                    };
                    reader.onerror = function() {
                    };
                });
            });
        }
        else
        {
            // TODO
        }
    });

    app.ports.write_file_to_js.subscribe((text) =>
        {
            try {
                if (existingHandle) {
                    existingHandle
                        .createWritable()
                        .then((writableStream) => {
                            writableStream.write(text).then((a) => {
                                writableStream.close();
                                app.ports.write_file_from_js.send(true);
                            });
                        })
                        .catch(error => app.ports.write_file_from_js.send(false));;
                }
                else {
                    window.showSaveFilePicker()
                        .then((newHandle) => {
                            existingHandle = newHandle;
                            newHandle.createWritable().then((writableStream) => {
                                    writableStream.write(text).then((a) => {
                                        writableStream.close();
                                        app.ports.write_file_from_js.send(true);
                                    });
                                });
                            })
                        .catch(error => app.ports.write_file_from_js.send(false));
                }
            } catch (err) {
                app.ports.write_file_from_js.send(false);
            }
        }
    )

    //app.ports.copy_to_clipboard_to_js.subscribe(text => copyTextToClipboard(text));

    function copyTextToClipboard(text) {
      if (!navigator.clipboard) {
        fallbackCopyTextToClipboard(text);
        return;
      }
      navigator.clipboard.writeText(text).then(function() {
        // console.log('Async: Copying to clipboard was successful!');
      }, function(err) {
        console.error('Error: Could not copy text: ', err);
      });
    }

    function fallbackCopyTextToClipboard(text) {
      var textArea = document.createElement("textarea");
      textArea.value = text;

      // Avoid scrolling to bottom
      textArea.style.top = "0";
      textArea.style.left = "0";
      textArea.style.position = "fixed";

      document.body.appendChild(textArea);
      textArea.focus();
      textArea.select();

      try {
        var successful = document.execCommand('copy');
        if (successful !== true) {
          console.log('Error: Copying text command was unsuccessful');
        }
      } catch (err) {
        console.error('Error: Oops, unable to copy', err);
      }

      document.body.removeChild(textArea);
    }
}