all: dclock.system.ca65 dclock.system.merlin32 compare

.PHONEY: compare clean

clean:
	$(RM) dclock.system.ca65     dclock.system.merlin32
	$(RM) dclock.system.ca65.lst dclock.system.merlin32_Output.txt
	$(RM) dclock.system.ca65.o
	$(RM) _FileInformation.txt
	$(RM) a b

dclock.system.ca65: dclock.system.ca65.s
	${CC65_HOME}/bin/ca65            dclock.system.ca65.s -l dclock.system.ca65.lst
	${CC65_HOME}/bin/ld65 -t none -o dclock.system.ca65      dclock.system.ca65.o

dclock.system.merlin32: dclock.system.merlin32.s
	merlin32 dclock.system.merlin32.s

compare: dclock.system.ca65 dclock.system.merlin32
	hexdump -C dclock.system.ca65     > a
	hexdump -C dclock.system.merlin32 > b
	diff a b


