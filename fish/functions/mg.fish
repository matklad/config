function mg
    e --eval "
        (progn 
            (call-interactively #'magit-status) 
            (call-interactively #'delete-other-windows))"
end
