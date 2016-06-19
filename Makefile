EMACS ?= emacs
CASK = env --unset INSIDE_EMACS EMACS=$(EMACS) cask

screenshot:
	$(CASK) exec $(EMACS) --debug-init -Q \
		-L . -l etc/screenshot.el
