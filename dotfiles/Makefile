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

all: bin bash emacs git xresources xmonad inputrc profile gdbinit vim i3 xprofile screenrc systemd gnupg catt waybar swayidle foot nwg-panel river mako claude ghostty

################################################################################

# Define a template function for directory copying rules
define copy_template
# Find source files
_$(1)files := $$(shell $$(FIND) $(1) -not -name '\#*\#' -type f -printf '%P\n')

# Define the target path based on destination type
$(2)/%: $(1)/%
	mkdir -p $$(@D)
	cp $$< $$@.tmp
	mv -f $$@.tmp $$@
	chmod $(3) $$@

# Target definition that expands to the pattern rule
$(1): $$(patsubst %,$(2)/%, $$(_$(1)files))
endef

$(eval $(call copy_template,bin,$(HOME)/bin,555))
$(eval $(call copy_template,bash,$(HOME)/.bash.d,444))
$(eval $(call copy_template,claude,$(HOME)/.claude,444))
$(eval $(call copy_template,emacs,$(HOME)/.emacs.d,444))
$(eval $(call copy_template,gnupg,$(HOME)/.gnupg,444))
$(eval $(call copy_template,xmonad,$(HOME)/.xmonad,400))

$(eval $(call copy_template,catt,$(HOME)/.config/catt,400))
$(eval $(call copy_template,waybar,$(HOME)/.config/waybar,400))
$(eval $(call copy_template,swayidle,$(HOME)/.config/swayidle,400))
$(eval $(call copy_template,foot,$(HOME)/.config/foot,400))
$(eval $(call copy_template,nwg-panel,$(HOME)/.config/nwg-panel,400))
$(eval $(call copy_template,river,$(HOME)/.config/river,500))
$(eval $(call copy_template,mako,$(HOME)/.config/mako,500))
$(eval $(call copy_template,ghostty,$(HOME)/.config/ghostty,400))

################################################################################

$(HOME)/.bashrc: bashrc/bashrc
	cp $< $@.tmp
	mv -f $@.tmp $@
	chmod 400 $@

bash: $(patsubst %,$(HOME)/.bash.d/%, $(_bashfiles)) $(HOME)/.bashrc

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

# Override systemd template behavior to add service management
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

.PHONY: all bin bash emacs git xresources xmonad profile inputrc gdbinit i3 xprofile ssh-agent screenrc systemd catt waybar swayidle foot nwg-panel river mako claude ghostty
