registerShortcut("Smart Close Window.",
    "Smart Close Window.",
    "win+q",
    function () {
        var c = workspace.activeClient;
        if (c.caption.indexOf("Firefox") == -1 && c.caption.indexOf("Vivaldi") == -1) {
            c.closeWindow();
        } else {
            c.minimized = true;
        }
    });
