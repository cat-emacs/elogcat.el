EMACS ?= emacs
BATCH = $(EMACS) -Q --batch
SOURCES = elogcat-core.el elogcat-filter.el elogcat-process.el elogcat.el
TEST_DEPS_DIR ?= .test-deps
PACKAGE_DIR = $(abspath $(TEST_DEPS_DIR)/elpa)
ADB_DIR ?= $(abspath $(TEST_DEPS_DIR)/adb)
ANDROID_PROJECT_DIR ?= $(abspath $(TEST_DEPS_DIR)/android-project)
LOAD_PATH = -L . -L "$(ADB_DIR)" -L "$(ANDROID_PROJECT_DIR)"

ARCHIVES = --eval "(setq package-user-dir \"$(PACKAGE_DIR)\")" \
	--eval "(require 'package)" \
	--eval "(add-to-list 'package-archives '(\"melpa\" . \"https://melpa.org/packages/\") t)" \
	--eval "(package-initialize)"

.PHONY: all install-deps compile test clean

all: clean compile

install-deps:
	mkdir -p "$(PACKAGE_DIR)"
	test -d "$(ADB_DIR)/.git" || git clone --depth 1 https://github.com/cat-emacs/adb "$(ADB_DIR)"
	test -d "$(ANDROID_PROJECT_DIR)/.git" || git clone --depth 1 https://github.com/cat-emacs/android-project "$(ANDROID_PROJECT_DIR)"
	$(BATCH) $(ARCHIVES) \
		--eval "(package-refresh-contents)" \
		--eval "(dolist (dependency '(dash s transient)) (unless (package-installed-p dependency) (package-install dependency)))"

compile:
	$(BATCH) $(ARCHIVES) $(LOAD_PATH) \
		--eval "(setq byte-compile-error-on-warn t)" \
		-f batch-byte-compile $(SOURCES)

test:
	$(BATCH) $(ARCHIVES) $(LOAD_PATH) \
		-l elogcat.el -l elogcat-tests.el \
		-f ert-run-tests-batch-and-exit

clean:
	rm -f *.elc
