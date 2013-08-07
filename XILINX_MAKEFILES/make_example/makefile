FPGA_PART     := xc2s50-5-tq144
fpga_projects := test_board ramintfc test_vga test_timer test_switches test_kbd test_codec

CPLD_PART     := xc9572xl-10-vq64
cpld_projects := erase fintf fcnfg dwnldpar dwnldpa2 p3jtag

export XST_CPLD_OPTIONS_FILE ?= $(CURDIR)/xst_cpld_options.xst
export XST_FPGA_OPTIONS_FILE ?= $(CURDIR)/xst_fpga_options.xst
export BITGEN_OPTIONS_FILE   ?= $(CURDIR)/bitgen_options.ut
export IMPACT_OPTIONS_FILE   ?= $(CURDIR)/impact_options.txt

config          : $(fpga_projects:=.config)           $(cpld_projects:=.config)
timing          : $(fpga_projects:=.timing)
clean           : $(fpga_projects:=.clean)            $(cpld_projects:=.clean)
distclean       : $(fpga_projects:=.distclean)        $(cpld_projects:=.distclean)
maintainer-clean: $(fpga_projects:=.maintainer-clean) $(cpld_projects:=.maintainer-clean)
nice            : $(fpga_projects:=.nice)             $(cpld_projects:=.nice)

$(fpga_projects:=.config):
	$(MAKE) -C $(subst .config,,$@) PART=$(FPGA_PART) $@

$(cpld_projects:=.config):
	$(MAKE) -C $(subst .config,,$@) PART=$(CPLD_PART) $@

$(fpga_projects:=.timing):
	$(MAKE) -C $(subst .timing,,$@) PART=$(FPGA_PART) $@

$(fpga_projects:=.clean) $(cpld_projects:=.clean):
	$(MAKE) -C $(subst .clean,,$@) $@

$(fpga_projects:=.distclean) $(cpld_projects:=.distclean):
	$(MAKE) -C $(subst .distclean,,$@) $@

$(fpga_projects:=.maintainer-clean) $(cpld_projects:=.maintainer-clean):
	$(MAKE) -C $(subst .maintainer-clean,,$@) $@

$(fpga_projects:=.nice) $(cpld_projects:=.nice):
	$(MAKE) -C $(subst .nice,,$@) $@

