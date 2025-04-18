local apps =
{
    F1 = "Ghostty",
    F2 = "Visual Studio Code",
    F3 = "Safari",
    F4 = "Slack",
    F5 = "Spotify",
}

for key, app in pairs(apps) do
    hs.hotkey.bind({"cmd"}, key, function()
        hs.application.launchOrFocus(app)
    end)
end
