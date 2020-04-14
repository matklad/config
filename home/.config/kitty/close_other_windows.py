def main(args):
   pass

def handle_result(args, answer, target_window_id, boss):
    active_window = boss.active_window
    if not active_window:
        return
    for window in boss.all_windows:
        if window != active_window:
            window.close()

handle_result.no_ui = True
