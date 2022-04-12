# uncomment to debug
# https://blog.jgc.org/2015/04/one-weird-trick-that-will-give-you.html
# _SHELL := $(SHELL)
# SHELL = $(warning [$@])$(_SHELL) -x

.DEFAULT: all

all: bin bash emacs ssh git upspin xresources xmonad inputrc profile aws grobi gdbinit vim i3 xprofile

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

_sshfiles=$(shell find ssh -not -name '\#*\#' -type f -printf '%P\n')
$(HOME)/.ssh/%: ssh/%
	mkdir -p $(@D)
	cp $< $@.tmp
	mv -f $@.tmp $@
	chmod 400 $@

ssh: $(patsubst %,$(HOME)/.ssh/%, $(_sshfiles))

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

$(HOME)/.gitconfig: git/config
	cp $< $@.tmp
	mv -f $@.tmp $@
	chmod 400 $@

$(HOME)/.gitconfig.d/local: git/config.$(shell hostname)
	mkdir -p $(@D)
	cp $< $@.tmp
	mv -f $@.tmp $@
	chmod 400 $@

$(HOME)/.gitconfig.d/personal: git/config.personal
	mkdir -p $(@D)
	cp $< $@.tmp
	mv -f $@.tmp $@
	chmod 400 $@

git: $(HOME)/.gitconfig $(HOME)/.gitconfig.d/local $(HOME)/.gitconfig.d/personal

$(HOME)/.Xresources: xresources/Xresources
	cp $< $@.tmp
	mv -f $@.tmp $@
	chmod 400 $@

xresources: $(HOME)/.Xresources

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

$(HOME)/.config/grobi.conf: grobi/grobi.conf
	mkdir -p $(@D)
	cp $< $@.tmp
	mv -f $@.tmp $@
	chmod 400 $@

grobi: $(HOME)/.config/grobi.conf


$(HOME)/.gdbinit: gdbinit/gdbinit
	cp $< $@.tmp
	mv -f $@.tmp $@
	chmod 400 $@

gdbinit: $(HOME)/.gdbinit


_upspinfiles=$(shell find upspin -not -name '\#*\#' -type f -printf '%P\n')
$(HOME)/.upspin/%: upspin/%
	mkdir -p $(@D)
	cp $< $@.tmp
	mv -f $@.tmp $@
	chmod 400 $@

upspin: $(patsubst %,$(HOME)/.upspin/%, $(_upspinfiles))

_awsfiles=$(shell find aws -not -name '\#*\#' -type f -printf '%P\n')
$(HOME)/.aws/%: aws/%
	mkdir -p $(@D)
	cp $< $@.tmp
	mv -f $@.tmp $@
	chmod 400 $@

aws: $(patsubst %,$(HOME)/.aws/%, $(_awsfiles))


$(HOME)/.config/i3/config: i3/config
	mkdir -p $(@D)
	cp $< $@.tmp
	mv -f $@.tmp $@
	chmod 400 $@

i3: $(HOME)/.config/i3/config

# https://blog.jgc.org/2015/04/the-one-line-you-should-add-to-every.html
print-%: ; @echo $* is $($*)

.PHONY: all bin bash emacs ssh git xresources xmonad upspin grobi aws profile inputrc gdbinit i3 xprofile ssh-agent
