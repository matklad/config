; IMPORTANT INFO ABOUT GETTING STARTED: Lines that start with a
; semicolon, such as this one, are comments.  They are not executed.

; See http://duartes.org/gustavo/blog/home-row-computing for more information on this script
; See the AutoHotKey docs at http://www.autohotkey.com/docs/  for AutoHotKey documentation
; Most of the syntax is described at http://www.autohotkey.com/docs/Hotkeys.htm


; AppsKey + jkl;

Appskey & i::Send {Blind}{Up DownTemp}
AppsKey & i up::Send {Blind}{Up Up}

AppsKey & k::Send {Blind}{Down DownTemp}
AppsKey & k up::Send {Blind}{Down Up}

AppsKey & j::Send {Blind}{Left DownTemp}
AppsKey & j up::Send {Blind}{Left Up}

AppsKey & l::Send {Blind}{Right DownTemp}
AppsKey & l up::Send {Blind}{Right Up}


; AppsKey + uiop

AppsKey & u::SendInput {Blind}{PgUp Down}
AppsKey & u up::SendInput {Blind}{PgUp Up}

AppsKey & o::SendInput {Blind}{PgDn Down}
AppsKey & o up::SendInput {Blind}{PgDn Up}

AppsKey & a::SendInput {Blind}{Home Down}
AppsKey & a up::SendInput {Blind}{Home Up}

AppsKey & e::SendInput {Blind}{End Down}
AppsKey & e up::SendInput {Blind}{End Up}


; AppsKey + dbwer

; AppsKey & ;::SendInput {Blind}{Del Down}
AppsKey & h::SendInput {Blind}{BS Down}

AppsKey & g::SendInput {Blind}{Esc Down}

; Make AppsKey & Enter equivalent to Control+Enter
AppsKey & n::SendInput {Enter}

; Make Windows Key + Apps Key work like Caps Lock
#AppsKey::Capslock