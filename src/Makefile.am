## Lisp normally uses ASDF for compiling big projects, an extensible make-like utility.
## ASDF also allows to build maxima on systems that don't contain autotools.
## It therefore is natural to use make only for initiating the build process
## and to use the autotools for setting up all files, but to use the standard lisp
## tools for the actual build.
##
## The ASDF equivalent of a makefile is named maxima.system.
## For details about its syntax see
## https://common-lisp.net/project/asdf/asdf/The-defsystem-form.html

include $(top_srcdir)/common.mk

if WIN32
bin_SCRIPTS = maxima-command.ico maxima.bat set_lang.vbs
else
bin_SCRIPTS = maxima rmaxima
endif

## In instsrcdir, we install the lisp source files in src/ (including
## the numerical subdirectories).
## TODO: Should we also be installing the .system and .asd files here?
real_lisp_sources := $(shell echo $(srcdir)/*.lisp $(srcdir)/numerical/*.lisp $(srcdir)/numerical/slatec/*.lisp)
nobase_dist_instsrc_DATA = $(real_lisp_sources)
EXTRA_DIST = \
  maxima.asd maxima.system \
  numerical/slatec/fortran \
  maxima-build.lisp maxima-command.ico set_lang.vbs \
  lisp \
  $(real_lisp_sources)

## If we can, we use Automake's blah_SCRIPTS targets to install
## stuff. When doing this, we have to populate EXTRA_SCRIPTS, so we
## use += and initialise here.
EXTRA_SCRIPTS =

## A debugging tool. If you're trying to understand the value of the
## variable $(foo) when make runs, call "make echo_foo" and it will be
## printed.
echo_%:
	@echo "$(subst echo_,,$@)=$($(subst echo_,,$@))"
	@echo "origin $(subst echo_,,$@) returns $(origin $(subst echo_,,$@))"

## "all" depends on sharefiles.mk and whatever files we choose to
## build. These are listed in BUILT_FILES, which is populated in the
## implementation-dependent sections below.
##
BUILT_FILES =
all-local: $(BUILT_FILES)

DISTCLEAN_FILES=$(DISTCLEAN_EXTRA_SRC_FILES)

## The "clean" rule always just deletes some files and they are
## specified in the conditional sections by adding
## implementation-specific targets to the list CLEAN_TARGETS.
CLEAN_TARGETS =
clean-local: $(CLEAN_TARGETS)

## Tell make clean to delete all the <foo>-depends.mk files.
DEPENDS_FILES = \
  clisp-depends.mk cmucl-depends.mk scl-depends.mk acl-depends.mk \
  sbcl-depends.mk gcl-depends.mk openmcl-depends.mk ecl-depends.mk abcl-depends.mk
CLEANFILES = $(DEPENDS_FILES) tools/sys-proclaim.lisp

## Similarly, we do something hacky with the install rule. Most of the
## time, we can actually use Automake's tools for this, but for the
## crazy "install another copy of the implementation" approach that we
## use with SCL and CLISP, we need to do it by hand.
WEIRD_INSTALL_TARGETS =
WEIRD_UNINSTALL_TARGETS =
install-exec-local: $(WEIRD_INSTALL_TARGETS)
uninstall-hook: $(WEIRD_UNINSTALL_TARGETS)

## A rule to build binary directories of the form binary-<lispname>
## and subdirectories ./numerical and ./numerical/slatec
binary_subdirs = / /numerical /numerical/slatec
lisps_enabled = @lisps_enabled@
.PHONY: bd
bd:
	for l in $(lisps_enabled); do for d in $(binary_subdirs); do $(MKDIR_P) binary-$$l$$d; done; done

## Some hunks of lisp that get used by more than one implementation
LOADDEFSYSTEM = (load "$(top_srcdir)/lisp-utils/defsystem.lisp") (mk:add-registry-location "$(top_srcdir)/src/")
LOADMAKEDEPENDS = (load "$(top_srcdir)/lisp-utils/make-depends.lisp")
DS_OOS = funcall (intern (symbol-name :operate-on-system) :mk) "maxima"
if QUIET_BUILD
QUIET="(setf *compile-print* nil) (setq *compile-verbose* nil)"
DEFSYSTEMCOMPILE = ($(DS_OOS) :compile :verbose nil)
DEFSYSTEMLOAD = ($(DS_OOS) :load :verbose nil )
else
# Set QUIET to an empty list, so that --eval '$(QUIET)' works.
QUIET="()"
DEFSYSTEMCOMPILE = ($(DS_OOS) :compile :verbose t)
DEFSYSTEMLOAD = ($(DS_OOS) :load :verbose t)
endif
DEFSYSTEMTESTLOAD = ($(DS_OOS) :load :verbose t :test t)

## A function that takes: $(1) = the target name or names (either a
## string or a list of strings); $(2) = <foo>-depends.mk; $(3) = any
## postscript that should be added. It outputs lisp code
## that, when run, should create the dependency Makefile fragments.
MAKE_DEPENDS = '$(LOADDEFSYSTEM) $(LOADMAKEDEPENDS) (funcall (intern "CREATE-DEPENDENCY-FILE" :mk) $(1) "$(2)") $(3)'

## CLISP #######################################################################
if CLISP

EXTRA_SCRIPTS += $(CLISP_MAXIMA)

if CLISP_EXEC
CLISP_MAXIMA = binary-clisp/maxima$(EXEEXT)
clisplib_SCRIPTS = $(CLISP_MAXIMA)
clispexeflag = :EXECUTABLE t
else
CLISP_MAXIMA = binary-clisp/maxima.mem
clisplib_DATA = $(CLISP_MAXIMA)
clispexeflag =
## Rather crazily, we install an extra copy of clisp. Maybe there's a
## more sensible approach than this?!
WEIRD_INSTALL_TARGETS += install-clisp-copy
WEIRD_UNINSTALL_TARGETS += uninstall-clisp-copy
install-clisp-copy:
	$(mkinstalldirs) $(DESTDIR)$(clisplibdir)
	$(INSTALL_PROGRAM) @CLISP_RUNTIME_PATH@ "$(DESTDIR)$(clisplibdir)/@CLISP_RUNTIME@"
uninstall-clisp-copy:
	rm -f "$(DESTDIR)$(clisplibdir)/@CLISP_RUNTIME@"
endif

EXECUTECLISP = $(CLISP_NAME) -norc -q
clisplibdir = $(verpkglibdir)/binary-clisp

BUILT_FILES += $(CLISP_MAXIMA)

clisp: $(CLISP_MAXIMA)

$(CLISP_MAXIMA):
	$(MAKE) bd
	echo '$(QUIET) $(LOADDEFSYSTEM) $(DEFSYSTEMCOMPILE)' > clisp-command.lisp
	$(EXECUTECLISP) clisp-command.lisp
	echo '$(LOADDEFSYSTEM) $(DEFSYSTEMLOAD) (ext:saveinitmem "$@" :init-function (function cl-user::run) $(clispexeflag))' > clisp-command.lisp
	$(EXECUTECLISP) clisp-command.lisp
	rm clisp-command.lisp

clisp-depends.mk: maxima.system Makefile *.lisp
	echo $(call MAKE_DEPENDS,"$(CLISP_MAXIMA)",clisp-depends.mk.tmp) > clisp-command2.lisp
	$(EXECUTECLISP) clisp-command2.lisp
	sed -e "s#\\\\#/#g" < clisp-depends.mk.tmp > clisp-depends.mk

-include clisp-depends.mk
endif CLISP

clean-clisp:
	rm -rf binary-clisp clisp-command.lisp clisp-command2.lisp
CLEAN_TARGETS += clean-clisp

## CMUCL #######################################################################
if CMUCL

EXTRA_SCRIPTS += $(CMU_MAXIMA) lisp

if CMUCL_EXEC
CMU_MAXIMA = binary-cmucl/maxima
cmucllib_SCRIPTS = $(CMU_MAXIMA)
cmuflag = :executable t :init-function '\''cl-user::run
else
CMU_MAXIMA = binary-cmucl/maxima.core
cmucllib_SCRIPTS = lisp
cmucllib_DATA = $(CMU_MAXIMA)
cmuflag =
endif

# Newer versions of CMUCL have an INTL package that is compatible with
# maxima's.  We just bind intl::*default-domain* here so that when we
# compile the files, we will get appropriate translations.  (Otherwise
# we have to put (intl:textdomain "maxima") in each Lisp file.
INITINTL = (when (find-package "INTL") (set (find-symbol "*DEFAULT-DOMAIN*" "INTL") "maxima"))
CMU_COMPILE = echo '$(QUIET) $(INITINTL) $(LOADDEFSYSTEM) $(DEFSYSTEMCOMPILE)' | $(EXECUTECMUCL)
CMU_WRITE = echo '$(LOADDEFSYSTEM) $(DEFSYSTEMLOAD) (ext:save-lisp "$(CMU_MAXIMA)" $(cmuflag))' | $(EXECUTECMUCL)
CMU_BUILD = ($(CMU_COMPILE)) && ($(CMU_WRITE))
EXECUTECMUCL = $(CMUCL_NAME) -noinit -batch

cmucllibdir = $(verpkglibdir)/binary-cmucl
BUILT_FILES += $(CMU_MAXIMA)

cmucl: $(CMU_MAXIMA)

$(CMU_MAXIMA):
	$(MAKE) bd
	$(CMU_BUILD)

cmucl-depends.mk: maxima.system Makefile *.lisp
	echo $(call MAKE_DEPENDS,"$(CMU_MAXIMA)",cmucl-depends.mk.tmp) | $(EXECUTECMUCL)
	sed -e "s#\\\\#/#g" < cmucl-depends.mk.tmp > cmucl-depends.mk

-include cmucl-depends.mk
endif

clean-cmucl:
	rm -rf binary-cmucl
CLEAN_TARGETS += clean-cmucl

## SCL #########################################################################
if SCL

scllibdir = $(verpkglibdir)/binary-scl

EXECUTESCL = $(SCL_NAME) -noinit -batch
BUILT_FILES += binary-scl/maxima.core
scllib_DATA = binary-scl/maxima.core

scl: binary-scl/maxima.core

binary-scl/maxima.core:
	$(MAKE) bd
	(echo '$(QUIET) $(LOADDEFSYSTEM) $(DEFSYSTEMCOMPILE)' | $(EXECUTESCL)) && \
	(echo '$(LOADDEFSYSTEM) $(DEFSYSTEMLOAD) (ext:save-lisp "binary-scl/maxima.core")' | $(EXECUTESCL))

## Just like with clisp, we install an extra copy of SCL. No, I don't
## understand either. Also, I just read the SCL license agreement. Is
## the user even allowed to do this?!
WEIRD_INSTALL_TARGETS += install-scl-copy
WEIRD_UNINSTALL_TARGETS += uninstall-scl-copy
install-scl-copy:
	$(mkinstalldirs) $(DESTDIR)$(scllibdir)
	$(INSTALL_PROGRAM) @SCL_RUNTIME_PATH@ "$(DESTDIR)$(scllibdir)/@SCL_RUNTIME@"
uninstall-scl-copy:
	rm -f "$(DESTDIR)$(scllibdir)/@SCL_RUNTIME@"

scl-depends.mk: maxima.system Makefile *.lisp
	echo $(call MAKE_DEPENDS,"binary-scl/maxima.core",scl-depends.mk.tmp) | $(EXECUTESCL)
	sed -e "s#\\\\#/#g" < scl-depends.mk.tmp > scl-depends.mk

-include scl-depends.mk

endif

clean-scl:
	rm -rf binary-scl
CLEAN_TARGETS += clean-scl

## ACL #########################################################################
if ACL

acllibdir = $(verpkglibdir)/binary-acl

EXECUTEACL = $(ACL_NAME) -batch
BUILT_FILES += binary-acl/maxima.dxl

acllib_DATA = binary-acl/maxima.dxl

acl: binary-acl/maxima.dxl

binary-acl/maxima.dxl:
	$(MAKE) bd
	(echo '$(QUIET) $(LOADDEFSYSTEM) $(DEFSYSTEMCOMPILE)' | $(EXECUTEACL)) && \
	(echo '$(LOADDEFSYSTEM) $(DEFSYSTEMLOAD) (excl:dumplisp :name "binary-acl/maxima.dxl")' | $(EXECUTEACL))

acl-depends.mk: maxima.system Makefile *.lisp
	echo $(call MAKE_DEPENDS,"binary-acl/maxima.dxl",acl-depends.mk.tmp) | $(EXECUTEACL)
	sed -e "s#\\\\#/#g" < acl-depends.mk.tmp > acl-depends.mk

-include acl-depends.mk
endif

clean-acl:
	rm -rf binary-acl
CLEAN_TARGETS += clean-acl

## SBCL ########################################################################
if SBCL

sbcllibdir = $(verpkglibdir)/binary-sbcl

EXTRA_SCRIPTS += $(SBCL_MAXIMA)

if SBCL_EXEC
SBCL_MAXIMA = binary-sbcl/maxima$(EXEEXT)
sbcllib_SCRIPTS = $(SBCL_MAXIMA)
sb_slad = (sb-ext:save-lisp-and-die "$@" :executable t)
else
SBCL_MAXIMA = binary-sbcl/maxima.core
sbcllib_DATA = $(SBCL_MAXIMA)
sb_slad = (sb-ext:save-lisp-and-die "$@")
endif

EXECUTESBCL = "$(SBCL_NAME)" --noinform --noprint $(SBCL_EXTRA_ARGS)
BUILT_FILES += $(SBCL_MAXIMA)

COMPILE_SBCL = echo '$(QUIET) $(LOADDEFSYSTEM) $(DEFSYSTEMCOMPILE) (sb-ext:quit)' | $(EXECUTESBCL) 
WRITE_SBCL = echo '$(LOADDEFSYSTEM) $(DEFSYSTEMLOAD) $(sb_slad) (sb-ext:quit)' | $(EXECUTESBCL)
BUILD_SBCL = $(COMPILE_SBCL) && $(WRITE_SBCL)

sbcl: $(SBCL_MAXIMA)

$(SBCL_MAXIMA):
	$(MAKE) bd
	$(BUILD_SBCL)

sbcl-depends.mk: maxima.system Makefile *.lisp
	echo $(call MAKE_DEPENDS,"$(SBCL_MAXIMA)",sbcl-depends.mk.tmp,(sb-ext:quit)) | $(EXECUTESBCL)
	sed -e "s#\\\\#/#g" < sbcl-depends.mk.tmp > sbcl-depends.mk

-include sbcl-depends.mk

endif

clean-sbcl:
	rm -rf binary-sbcl numerical/binary-sbcl numerical/slatec/binary-sbcl
CLEAN_TARGETS += clean-sbcl

## GCL #########################################################################
#
# For gcl we execute all commands from a temp file for the following motive:
#
# Some versions of GCL 2.6.12 don't automatically close at the end of a lisp
# file that contains the commands needed for compiling maxima and crash if
# they reach a (quit). In newer versions of 2.6.12 sending the (quit) from the
# command-line causes a crash.
# GCL's --eval switch adds an implicit (progn) that breaks defsystem's
# (mk:add-registry-location).
# And when executing commands from stdin some GCL versions get the
# end-of-file from the end of the command list too early and therefore close
# before the last command has been processed.
# Providing all commands from the command-line and ending them with a
# (quit) so it doesn't matter if lisp automatically closes at the last
# or the second last command seems to work, though.

if GCL

gcllibdir = $(verpkglibdir)/binary-gcl

EXTRA_SCRIPTS += binary-gcl/maxima
gcllib_SCRIPTS = binary-gcl/maxima

EXECUTEGCL = $(GCL_NAME)
BUILT_FILES += binary-gcl/maxima

sys_proc_dependency = tools/sys-proclaim.lisp
gcl_depends_targets = (list "binary-gcl/maxima" "tools/sys-proclaim.lisp")

tools/sys-proclaim.lisp: maxima.system *.lisp
	rm -rf binary-gcl
	touch sys-proclaim.lisp
	$(MAKE) bd
	echo '(load "generate-sys-proclaim.lisp") (system::quit)' | $(EXECUTEGCL)
	rm -rf binary-gcl
	mkdir -p tools
	mv sys-proclaim.lisp tools

gcl: binary-gcl/maxima

binary-gcl/maxima: $(sys_proc_dependency) gcl-depends.mk
	$(MAKE) bd
if GCL_ALT_LINK
	echo '$(QUIET) $(LOADDEFSYSTEM) $(DEFSYSTEMCOMPILE) (system::quit)' | $(EXECUTEGCL)
	echo '(let ((com (quote (progn (defvar compiler::*gazonk-prefix* "gazonk") (defun compiler::gazonk-name (&aux tem)(dotimes (i 1000) (unless (probe-file (setq tem (merge-pathnames (format nil "~d~d.lsp" compiler::*gazonk-prefix* i))))(return-from compiler::gazonk-name (pathname tem))))(error "1000 gazonk names used already!"))(let ((compiler::*gazonk-prefix* "maxima_gazonk")(compiler::*keep-gaz* t))$(LOADDEFSYSTEM)$(DEFSYSTEMLOAD))))))(let ((si::*collect-binary-modules* t))(eval com)(let ((compiler::*default-system-p* t))(dolist (l (directory "maxima_gazonk*.lsp")) (compile-file l) (delete-file l)))(compiler::link si::*binary-modules* "binary-gcl/maxima" (format nil "~S" com) "" nil)(dolist (l (directory "maxima_gazonk*.lsp")) (delete-file l)))) (system::quit)' | $(EXECUTEGCL)
else
	echo '$(LOADDEFSYSTEM) $(DEFSYSTEMCOMPILE) (system::quit)' | $(EXECUTEGCL)
	echo '(setq si::*optimize-maximum-pages* nil) $(LOADDEFSYSTEM) $(DEFSYSTEMLOAD) (setq si::*optimize-maximum-pages* t) (si:save-system "binary-gcl/maxima") (system::quit)'  | $(EXECUTEGCL)
endif

gcl-depends.mk: maxima.system Makefile *.lisp
	echo $(call MAKE_DEPENDS,$(gcl_depends_targets),gcl-depends.mk.tmp) '(system::quit)' | $(EXECUTEGCL)
	sed -e "s#\\\\#/#g" < gcl-depends.mk.tmp > gcl-depends.mk
-include gcl-depends.mk

endif

clean-gcl:
	rm -rf binary-gcl
CLEAN_TARGETS += clean-gcl

## OPENMCL / CCL ###############################################################
if OPENMCL

openmcllibdir = $(verpkglibdir)/binary-openmcl
EXTRA_SCRIPTS += $(OPENMCL_MAXIMA)

if OPENMCL_EXEC
OPENMCL_MAXIMA = binary-openmcl/maxima$(EXEEXT)
openmcllib_SCRIPTS = $(OPENMCL_MAXIMA)
omcl_flag = :prepend-kernel t
else
OPENMCL_MAXIMA = binary-openmcl/maxima.image
openmcllib_DATA = $(OPENMCL_MAXIMA)
omcl_flag =
endif

EXECUTEOPENMCL = $(OPENMCL_NAME)
BUILT_FILES += $(OPENMCL_MAXIMA)

COMPILE_OMCL = echo '$(QUIET) (require :defsystem) (mk:add-registry-location "$(top_srcdir)/src/") $(DEFSYSTEMCOMPILE) (ccl::quit)' | $(EXECUTEOPENMCL)
WRITE_OMCL = echo '(require :defsystem) (mk:add-registry-location "$(top_srcdir)/src/") $(DEFSYSTEMLOAD) (ccl:save-application "$@") (ccl::quit)' | $(EXECUTEOPENMCL)
BUILD_OMCL = $(COMPILE_OMCL) && $(WRITE_OMCL)

openmcl: $(OPENMCL_MAXIMA)

$(OPENMCL_MAXIMA):
	$(BUILD_OMCL)
	echo '(require :defsystem)  $(DEFSYSTEMCOMPILE) (ccl::quit)' | $(EXECUTEOPENMCL) \
	   && \
	echo '(require :defsystem) $(DEFSYSTEMLOAD) (ccl:save-application "$@" $(omcl_flag)) (ccl::quit)' | $(EXECUTEOPENMCL)

openmcl-depends.mk: maxima.system Makefile *.lisp
	echo $(call MAKE_DEPENDS,"$(OPENMCL_MAXIMA)",openmcl-depends.mk.tmp,(ccl:quit)) | $(EXECUTEOPENMCL)
	sed -e "s#\\\\#/#g" < openmcl-depends.mk.tmp > openmcl-depends.mk

-include openmcl-depends.mk

endif

clean-openmcl:
	rm -rf binary-openmcl
CLEAN_TARGETS += clean-openmcl

## CCL64 #######################################################################

if CCL64

ccl64libdir = $(verpkglibdir)/binary-ccl64
EXTRA_SCRIPTS += $(OPENMCL_MAXIMA)

if CCL64_EXEC
CCL64_MAXIMA = binary-ccl64/maxima$(EXEEXT)
ccl64lib_SCRIPTS = $(CCL64_MAXIMA)
omcl_flag = :prepend-kernel t
else
CCL64_MAXIMA = binary-ccl64/maxima.image
ccl64lib_DATA = $(CCL64_MAXIMA)
omcl_flag =
endif

EXECUTECCL64 = $(CCL64_NAME)
BUILT_FILES += $(CCL64_MAXIMA)

COMPILE_CCL64 = $(EXECUTECCL64) --eval '$(QUIET)' --eval '(require :defsystem)' --eval '(mk:add-registry-location "$(top_srcdir)/src/")' --eval '$(DEFSYSTEMCOMPILE)' --eval '(ccl::quit)'
WRITE_CCL64 = $(EXECUTECCL64) --eval '(require :defsystem)' --eval '(mk:add-registry-location "$(top_srcdir)/src/")' --eval '$(DEFSYSTEMLOAD)' --eval '(ccl:save-application "$@")' --eval '(ccl::quit)'
BUILD_CCL64 = $(COMPILE_CCL64) && $(WRITE_CCL64)

ccl64: $(CCL64_MAXIMA)

$(CCL64_MAXIMA):
	$(BUILD_CCL64)
	$(EXECUTECCL64) --eval '(require :defsystem)' --eval '$(DEFSYSTEMCOMPILE)' --eval '(ccl::quit)' && \
	$(EXECUTECCL64) --eval '(require :defsystem)' --eval '$(DEFSYSTEMLOAD)' --eval '(ccl:save-application "$@" $(omcl_flag))' --eval '(ccl::quit)'

ccl64-depends.mk: maxima.system Makefile *.lisp
	echo $(call MAKE_DEPENDS,"$(CCL64_MAXIMA)",ccl64-depends.mk.tmp,(ccl:quit)) >ccl64-depends.mk.tmp
	$(EXECUTECCL64) --load ccl64-depends.mk.tmp | sed -e "s#\\\\#/#g" > ccl64-depends.mk

-include ccl64-depends.mk


endif

clean-ccl64:
	rm -rf binary-ccl64
CLEAN_TARGETS += clean-ccl64

## ECL #########################################################################
if ECL

ecllibdir = $(verpkglibdir)/binary-ecl
EXTRA_SCRIPTS += binary-ecl/maxima
ecllib_SCRIPTS = binary-ecl/maxima

EXECUTEECL = $(ECL_NAME) -norc
BUILT_FILES += binary-ecl/maxima

ecl:binary-ecl/maxima

binary-ecl/maxima:
	$(MAKE) bd
	echo '$(QUIET) $(LOADDEFSYSTEM) $(DEFSYSTEMCOMPILE) (build-maxima-lib) (ext:quit)' | $(EXECUTEECL)

ecl-depends.mk: maxima.system Makefile *.lisp
	echo $(call MAKE_DEPENDS,"binary-ecl/maxima",ecl-depends.mk.tmp,(quit)) | $(EXECUTEECL)
	sed -e "s#\\\\#/#g" < ecl-depends.mk.tmp > ecl-depends.mk

-include ecl-depends.mk
endif

clean-ecl:
	rm -rf binary-ecl libmaxima.a
CLEAN_TARGETS += clean-ecl

## ABCL ########################################################################
if ABCL

# ABCL compiles the individual files maxima is comprised of. But they aren't 
# compiled into a single .jar archive, but need to be loaded by abcl in the
# right order => We need to copy all the stuff defsystem needs into the binary
# folder and need to use defsystem for loading all the files in the Right Order.

abcllibdir = $(verpkglibdir)/binary-abcl
EXTRA_SCRIPTS += binary-abcl/maxima
abcllib_SCRIPTS = binary-abcl/maxima

EXECUTEABCL = $(JRE) -jar $(ABCL_JAR) --noinit
BUILT_FILES += binary-abcl/maxima

WEIRD_INSTALL_TARGETS += install-abcl-folder
WEIRD_UNINSTALL_TARGETS += uninstall-abcl-folder

install-abcl-folder:
	$(mkinstalldirs) $(DESTDIR)$(abcllibdir)
	cp -r binary-abcl/* "$(DESTDIR)$(abcllibdir)"
	chmod +x $(DESTDIR)$(abcllibdir)/maxima
uninstall-abcl-folder:
	if test x"$(abcllibdir)" = x"" ; then echo "Error: No known maxima version"; exit -1; else rm -f "$(DESTDIR)$(abcllibdir)/*"; fi

abcl:binary-abcl/maxima


binary-abcl/maxima: Makefile.am startmaxima_abcl.sh
	$(MAKE) bd
if QUIET_BUILD
	$(EXECUTEABCL) --eval '(setq *compile-verbose* nil)' --eval '(setf *compile-print* nil)' --eval '(load "$(top_srcdir)/lisp-utils/defsystem.lisp")' --eval '(mk:add-registry-location "$(top_srcdir)/src/")' --eval '($(DS_OOS) :compile :verbose nil)' --eval '(quit)'
else
	$(EXECUTEABCL) --eval '(load "$(top_srcdir)/lisp-utils/defsystem.lisp")' --eval '(mk:add-registry-location "$(top_srcdir)/src/")' --eval '($(DS_OOS) :compile :verbose t)' --eval '(quit)'
endif
	sed -e 's#"binary-abcl"#(ext:getenv "MAXIMA_IMAGESDIR_BIN")#g' < $(srcdir)/maxima.system > binary-abcl/maxima.system
	cp $(srcdir)/maxima-package.lisp binary-abcl
	cp $(srcdir)/autol.lisp binary-abcl
	cp $(srcdir)/init-cl.lisp binary-abcl
	cp $(srcdir)/max_ext.lisp binary-abcl
	cp autoconf-variables.lisp binary-abcl
	cp $(ABCL_JAR) binary-abcl/abcl.jar
	cp $(srcdir)/../lisp-utils/defsystem.lisp binary-abcl
	cp -a startmaxima_abcl.sh binary-abcl/maxima
	touch binary-abcl/maxima
	chmod +x binary-abcl/maxima

abcl-depends.mk: maxima.system Makefile *.lisp
	echo $(call MAKE_DEPENDS,"binary-abcl/maxima",abcl-depends.mk.tmp,(quit)) | $(EXECUTEABCL)
	sed -e "s#\\\\#/#g" < abcl-depends.mk.tmp > abcl-depends.mk

-include abcl-depends.mk
endif

clean-abcl:
	rm -rf binary-abcl libmaxima.a
CLEAN_TARGETS += clean-abcl

distclean-local:
	rm -f sbcl-depends.mk.tmp
