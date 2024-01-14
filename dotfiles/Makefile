# uncomment to debug
# https://blog.jgc.org/2015/04/one-weird-trick-that-will-give-you.html
# _SHELL := $(SHELL)
# SHELL = $(warning [$@])$(_SHELL) -x

.DEFAULT: all

all: bin bash emacs git xresources xmonad inputrc profile gdbinit vim i3 xprofile screenrc systemd gnupg catt hyprland waybar swayidle foot nwg-panel

_binfiles:=$(shell find bin -not -name '\#*\#' -type f -printf '%P\n')
$(HOME)/bin/%: bin/%
	mkdir -p $(@D)
	cp $< $@.tmp
	mv -f $@.tmp $@
	chmod 555 $@

bin: $(patsubst %,$(HOME)/bin/%, $(_binfiles))

_bashfiles=$(shell find bash -not -name '\#*\#' -type f -printf '%P\n')
$(HOME)/.bash.d/%: bash/%
	mkdir -p $(@D)
	cp $< $@.tmp
	mv -f $@.tmp $@
	chmod 444 $@

$(HOME)/.bashrc: bashrc/bashrc
	cp $< $@.tmp
	mv -f $@.tmp $@
	chmod 400 $@

bash: $(patsubst %,$(HOME)/.bash.d/%, $(_bashfiles)) $(HOME)/.bashrc

_emacsfiles=$(shell find emacs -not -name '\#*\#' -type f -printf '%P\n')
$(HOME)/.emacs.d/%: emacs/%
	mkdir -p $(@D)
	cp $< $@.tmp
	mv -f $@.tmp $@
	chmod 444 $@

emacs: $(patsubst %,$(HOME)/.emacs.d/%, $(_emacsfiles))

_gnupgfiles=$(shell find gnupg -not -name '\#*\#' -type f -printf '%P\n')
$(HOME)/.gnupg/%: gnupg/%
	mkdir -p $(@D)
	cp $< $@.tmp
	mv -f $@.tmp $@
	chmod 444 $@

gnupg: $(patsubst %,$(HOME)/.gnupg/%, $(_gnupgfiles))

$(HOME)/.config/systemd/user/ssh-agent.service: ssh-agent/ssh-agent.service
	mkdir -p $(@D)
	cp $< $@.tmp
	mv -f $@.tmp $@
	chmod 400 $@
	systemctl --user enable ssh-agent
	systemctl --user start ssh-agent

ssh-agent: $(HOME)/.config/systemd/user/ssh-agent.service

_xmonadfiles=$(shell find xmonad -not -name '\#*\#' -type f -printf '%P\n')
$(HOME)/.xmonad/%: xmonad/%
	mkdir -p $(@D)
	cp $< $@.tmp
	mv -f $@.tmp $@
	chmod 400 $@

xmonad: $(patsubst %,$(HOME)/.xmonad/%, $(_xmonadfiles))

_cattfiles=$(shell find catt -not -name '\#*\#' -type f -printf '%P\n')
$(HOME)/.config/catt/%: catt/%
	mkdir -p $(@D)
	cp $< $@.tmp
	mv -f $@.tmp $@
	chmod 400 $@

catt: $(patsubst %,$(HOME)/.config/catt/%, $(_cattfiles))

$(HOME)/.config/hypr/local.conf: $(shell find hyprland \( -name $(shell hostname).conf -o -name default \) -print -quit)
	mkdir -p $(@D)
	cp $< $@.tmp
	mv -f $@.tmp $@
	chmod 400 $@

_hyprlandfiles=$(shell find hyprland -not -name '\#*\#' -type f -printf '%P\n')
$(HOME)/.config/hypr/%: hyprland/%
	mkdir -p $(@D)
	cp $< $@.tmp
	mv -f $@.tmp $@
	chmod 400 $@

hyprland: $(patsubst %,$(HOME)/.config/hypr/%, $(_hyprlandfiles)) $(HOME)/.config/hypr/local.conf

_waybarfiles=$(shell find waybar -not -name '\#*\#' -type f -printf '%P\n')
$(HOME)/.config/waybar/%: waybar/%
	mkdir -p $(@D)
	cp $< $@.tmp
	mv -f $@.tmp $@
	chmod 400 $@

waybar: $(patsubst %,$(HOME)/.config/waybar/%, $(_waybarfiles))

_nwg-panelfiles=$(shell find nwg-panel -not -name '\#*\#' -type f -printf '%P\n')
$(HOME)/.config/nwg-panel/%: nwg-panel/%
	mkdir -p $(@D)
	cp $< $@.tmp
	mv -f $@.tmp $@
	chmod 400 $@

nwg-panel: $(patsubst %,$(HOME)/.config/nwg-panel/%, $(_nwg-panelfiles))

_swayidlefiles=$(shell find swayidle -not -name '\#*\#' -type f -printf '%P\n')
$(HOME)/.config/swayidle/%: swayidle/%
	mkdir -p $(@D)
	cp $< $@.tmp
	mv -f $@.tmp $@
	chmod 400 $@

swayidle: $(patsubst %,$(HOME)/.config/swayidle/%, $(_swayidlefiles))

_footfiles=$(shell find foot -not -name '\#*\#' -type f -printf '%P\n')
$(HOME)/.config/foot/%: foot/%
	mkdir -p $(@D)
	cp $< $@.tmp
	mv -f $@.tmp $@
	chmod 400 $@

foot: $(patsubst %,$(HOME)/.config/foot/%, $(_footfiles))

$(HOME)/.gitconfig: git/config
	cp $< $@.tmp
	mv -f $@.tmp $@
	chmod 400 $@

git: $(HOME)/.gitconfig

$(HOME)/.Xresources: xresources/Xresources
	cp $< $@.tmp
	mv -f $@.tmp $@
	chmod 400 $@
	xrdb -merge $@

xresources: $(HOME)/.Xresources

$(HOME)/.Xresources.d/local: $(shell find xresources.d \( -name $(shell hostname) -o -name default \) -print -quit)
	mkdir -p $(@D)
	cp $< $@.tmp
	mv -f $@.tmp $@
	chmod 400 $@

xresources: $(HOME)/.Xresources $(HOME)/.Xresources.d/local

$(HOME)/.vimrc: vim/vimrc
	cp $< $@.tmp
	mv -f $@.tmp $@
	chmod 400 $@
vim: $(HOME)/.vimrc

$(HOME)/.inputrc: inputrc/inputrc
	cp $< $@.tmp
	mv -f $@.tmp $@
	chmod 400 $@

inputrc: $(HOME)/.inputrc

$(HOME)/.profile: profile/profile
	cp $< $@.tmp
	mv -f $@.tmp $@
	chmod 400 $@

profile: $(HOME)/.profile $(HOME)/.xprofile

$(HOME)/.xprofile: profile/profile
	cp $< $@.tmp
	mv -f $@.tmp $@
	chmod 400 $@

xprofile: $(HOME)/.xprofile

$(HOME)/.gdbinit: gdbinit/gdbinit
	cp $< $@.tmp
	mv -f $@.tmp $@
	chmod 400 $@

gdbinit: $(HOME)/.gdbinit

$(HOME)/.screenrc: screenrc/screenrc
	cp $< $@.tmp
	mv -f $@.tmp $@
	chmod 400 $@

screenrc: $(HOME)/.screenrc

_systemdfiles=$(shell find systemd -not -name '\#*\#' -type f -printf '%P\n')
$(HOME)/.config/systemd/user/%: systemd/%
	mkdir -p $(@D)
	cp $< $@.tmp
	mv -f $@.tmp $@
	chmod 444 $@
	systemctl --user daemon-reload
	systemctl --user enable $(@F)
	systemctl --user start $(@F)

systemd: $(patsubst %,$(HOME)/.config/systemd/user/%, $(_systemdfiles))

# https://blog.jgc.org/2015/04/the-one-line-you-should-add-to-every.html
print-%: ; @echo $* is $($*)

.PHONY: all bin bash emacs git xresources xmonad profile inputrc gdbinit i3 xprofile ssh-agent screenrc systemd catt hyprland waybar swayidle foot nwg-panel
