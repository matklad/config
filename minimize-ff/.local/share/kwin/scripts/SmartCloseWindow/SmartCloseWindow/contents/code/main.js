registerShortcut("Smart Close Window.",
    "Smart Close Window.",
    "alt+f5",
    function () {
        var c = workspace.activeClient;
        if (c.caption.indexOf("Firefox") == -1) {
            c.closeWindow();
        } else {
            c.minimized = true;
        }
    });
