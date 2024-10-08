function fish_prompt --description 'Write out the prompt'
    set -l last_pipestatus $pipestatus
    set -lx __fish_last_status $status # Export for __fish_print_pipestatus.
    set -l normal (set_color normal)
    set -q fish_color_status
    or set -g fish_color_status red

    # Color the prompt differently when we're root
    set -l color_cwd $fish_color_cwd
    set -l suffix 'λ'
    if functions -q fish_is_root_user; and fish_is_root_user
        if set -q fish_color_cwd_root
            set color_cwd $fish_color_cwd_root
        end
        set suffix '#'
    end

    # Write pipestatus
    # If the status was carried over (if no command is issued or if `set` leaves the status untouched), don't bold it.
    set -l bold_flag --bold
    set -q __fish_prompt_status_generation; or set -g __fish_prompt_status_generation $status_generation
    if test $__fish_prompt_status_generation = $status_generation
        set bold_flag
    end
    set __fish_prompt_status_generation $status_generation
    set -l status_color (set_color $fish_color_status)
    set -l statusb_color (set_color $bold_flag $fish_color_status)
    set -l prompt_status (__fish_print_pipestatus "[" "]" "|" "$status_color" "$statusb_color" $last_pipestatus)

    echo -s (prompt_login)' ' (set_color $color_cwd) (prompt_pwd) $normal (fish_vcs_prompt) $normal " " $prompt_status
    echo -n -s $suffix ' '
end

# function fish_prompt --description 'Write out the prompt'
#     set -l last_status $status
#     echo

#     set_color normal
#     echo -n (date '+%X|')

#     # PWD
#     set_color --bold $fish_color_cwd
#     echo -n (prompt_pwd)
#     set_color normal

#     __terlar_git_prompt
#     __fish_hg_prompt
#     echo

#     if not test $last_status -eq 0
#         set_color $fish_color_error
#     end

#     if set -q SSH_TTY
#         echo -n "$hostname "
#     end
#     if set -q FHS
#         echo -n "(fhs) "
#     end

#     echo -n 'λ '
#     set_color normal
# end
