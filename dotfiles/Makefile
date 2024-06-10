# uncomment to debug
# https://blog.jgc.org/2015/04/one-weird-trick-that-will-give-you.html
# _SHELL := $(SHELL)
# SHELL = $(warning [$@])$(_SHELL) -x


UNAME_S := $(shell uname -s)
ifeq ($(UNAME_S),Darwin)
	FIND=gfind
else
	FIND=find
endif

.DEFAULT: all

all: bin bash emacs git xresources xmonad inputrc profile gdbinit vim i3 xprofile screenrc systemd gnupg catt hyprland waybar swayidle foot nwg-panel river mako

_binfiles:=$(shell $(FIND) bin -not -name '\#*\#' -type f -printf '%P\n')
$(HOME)/bin/%: bin/%
	mkdir -p $(@D)
	cp $< $@.tmp
	mv -f $@.tmp $@
	chmod 555 $@

bin: $(patsubst %,$(HOME)/bin/%, $(_binfiles))

_bashfiles=$(shell $(FIND) bash -not -name '\#*\#' -type f -printf '%P\n')
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

_emacsfiles=$(shell $(FIND) emacs -not -name '\#*\#' -type f -printf '%P\n')
$(HOME)/.emacs.d/%: emacs/%
	mkdir -p $(@D)
	cp $< $@.tmp
	mv -f $@.tmp $@
	chmod 444 $@

emacs: $(patsubst %,$(HOME)/.emacs.d/%, $(_emacsfiles))

_gnupgfiles=$(shell $(FIND) gnupg -not -name '\#*\#' -type f -printf '%P\n')
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

_xmonadfiles=$(shell $(FIND) xmonad -not -name '\#*\#' -type f -printf '%P\n')
$(HOME)/.xmonad/%: xmonad/%
	mkdir -p $(@D)
	cp $< $@.tmp
	mv -f $@.tmp $@
	chmod 400 $@

xmonad: $(patsubst %,$(HOME)/.xmonad/%, $(_xmonadfiles))

_cattfiles=$(shell $(FIND) catt -not -name '\#*\#' -type f -printf '%P\n')
$(HOME)/.config/catt/%: catt/%
	mkdir -p $(@D)
	cp $< $@.tmp
	mv -f $@.tmp $@
	chmod 400 $@

catt: $(patsubst %,$(HOME)/.config/catt/%, $(_cattfiles))

$(HOME)/.config/hypr/local.conf: $(shell $(FIND) hyprland \( -name $(shell hostname).conf -o -name default \) -print -quit)
	mkdir -p $(@D)
	cp $< $@.tmp
	mv -f $@.tmp $@
	chmod 400 $@

_hyprlandfiles=$(shell $(FIND) hyprland -not -name '\#*\#' -type f -printf '%P\n')
$(HOME)/.config/hypr/%: hyprland/%
	mkdir -p $(@D)
	cp $< $@.tmp
	mv -f $@.tmp $@
	chmod 400 $@

hyprland: $(patsubst %,$(HOME)/.config/hypr/%, $(_hyprlandfiles)) $(HOME)/.config/hypr/local.conf

_waybarfiles=$(shell $(FIND) waybar -not -name '\#*\#' -type f -printf '%P\n')
$(HOME)/.config/waybar/%: waybar/%
	mkdir -p $(@D)
	cp $< $@.tmp
	mv -f $@.tmp $@
	chmod 400 $@

waybar: $(patsubst %,$(HOME)/.config/waybar/%, $(_waybarfiles))

_nwg-panelfiles=$(shell $(FIND) nwg-panel -not -name '\#*\#' -type f -printf '%P\n')
$(HOME)/.config/nwg-panel/%: nwg-panel/%
	mkdir -p $(@D)
	cp $< $@.tmp
	mv -f $@.tmp $@
	chmod 400 $@

nwg-panel: $(patsubst %,$(HOME)/.config/nwg-panel/%, $(_nwg-panelfiles))

_swayidlefiles=$(shell $(FIND) swayidle -not -name '\#*\#' -type f -printf '%P\n')
$(HOME)/.config/swayidle/%: swayidle/%
	mkdir -p $(@D)
	cp $< $@.tmp
	mv -f $@.tmp $@
	chmod 400 $@

swayidle: $(patsubst %,$(HOME)/.config/swayidle/%, $(_swayidlefiles))

_footfiles=$(shell $(FIND) foot -not -name '\#*\#' -type f -printf '%P\n')
$(HOME)/.config/foot/%: foot/%
	mkdir -p $(@D)
	cp $< $@.tmp
	mv -f $@.tmp $@
	chmod 400 $@

foot: $(patsubst %,$(HOME)/.config/foot/%, $(_footfiles))

_riverfiles=$(shell $(FIND) river -not -name '\#*\#' -type f -printf '%P\n')
$(HOME)/.config/river/%: river/%
	mkdir -p $(@D)
	cp $< $@.tmp
	mv -f $@.tmp $@
	chmod 500 $@

river: $(patsubst %,$(HOME)/.config/river/%, $(_riverfiles))

_makofiles=$(shell $(FIND) mako -not -name '\#*\#' -type f -printf '%P\n')
$(HOME)/.config/mako/%: mako/%
	mkdir -p $(@D)
	cp $< $@.tmp
	mv -f $@.tmp $@
	chmod 500 $@

mako: $(patsubst %,$(HOME)/.config/mako/%, $(_makofiles))

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

$(HOME)/.Xresources.d/local: $(shell $(FIND) xresources.d \( -name $(shell hostname) -o -name default \) -print -quit)
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

_systemdfiles=$(shell $(FIND) systemd -not -name '\#*\#' -type f -printf '%P\n')
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
