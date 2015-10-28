OS2INFO Description
===================
OS2INFO allows to list details about your running OS/2 or eComStation
environment and also shows hardware related information (CPU, Number
of CPU's installed).
An example of that can be found at the end of this document.

If you call OS2INFO without any parameters it will display all information.
If you call it with the parameters "uptime memory" it will display the 
current uptime and the installed/free memory.
The uptime will be shown with three different values.
1. CPU count (may vary a little bit)
2. Default OS/2 and eCS uptime function (overflow after 49.7 days)
3. Swapper.dat creation date
   There seems to be a problem is HPFS386 because the timestamp won't
   be reset after a restart --> see example output at the end of this
   document!


OS2INFO Help
============
You call the usage screen by using the parameter "-?", "--help" or any 
unknown parameter!


License
=======
The source (Virtual Pascal) is included in this package and is distributed 
using the GPL! The program is freeware!
This software is provided "as is," without warranty of any kind, express
or implied.  In no event shall OS2INFO or its contributors be held liable
for any direct, indirect, incidental, special or consequential damages
arising out of the use of or inability to use this software.


Virtual Pascal
==============
The Virtual Pascal compiler can be downloaded free of charge at:
http://www.vpascal.com/


DOWNLOAD & CONTACT
==================
The latest version can be found on my website:
http://www.juergen-ulbts.de/

Juergen Ulbts (Germany in June 2006)




Example output of my current desktop machine:
=============================================
[d:\os2prog\pascal\projects\os2info]os2info
SystemUptime1:00d 02h 20m 54s
SystemUptime2:00d 02h 21m 39s
SystemUptime3:238d 00h 23m 41s          <--- SWAPPER.DAT Timestamp 
                                             (see comment above)

SystemVersion:OS/2 v20.45
KernelVersion:14.098b_W4
SystemMainVersion:eComStation

SystemProcessors:1

SystemMemory:
TotalPhysicalMem:1048128KB
TotalAvailMem:830460KB

SystemSwap:
TotalAvailSwap:1497880KB
TotalUsedSwap:2048KB

SystemDriveList:
Partition:C Mount:/ Type:HPFS PartSize:2562366KB PartAvail:1497880KB
Partition:D Mount:/ Type:HPFS PartSize:14337980KB PartAvail:86208KB
Partition:E Mount:/ Type:JFS PartSize:10200088KB PartAvail:4540060KB
Partition:G Mount:/ Type:HPFS PartSize:5124734KB PartAvail:2501683KB
Partition:L Mount:/ Type:JFS PartSize:22489436KB PartAvail:2561132KB
Partition:S Mount:/ Type:FAT PartSize:1424KB PartAvail:1424KB

SystemPhysicalDriveList:
EIDE: HD_0 Hard Drive FIXED DISK
EIDE: HD_1 Hard Drive FIXED DISK
EIDE: ATAPI_0 IDE DISK Drive REMOVABLE DISK
EIDE: ATAPI_0 IDE DISK Drive REMOVABLE DISK

SystemNetworkInfo:
lan0 received:13463KB sent:965KB
lan1 received:0KB sent:0KB
loopback received:0KB sent:291KB

ProcName:AuthenticAMD
ExtProcName=AMD Athlon(tm) XP 2500+                         
CPU=Generation:    0 
L1CodeCache=L1 instruction cache:    50432 KB 
L1DataCache=L1 data cache:    50432 KB 
L2=L2 cache:    50432 KB 
Family=5 
Model=0 
Stepping=0 
Extension=0 
CPUSpeed: ~1822 MHz
=============================================
