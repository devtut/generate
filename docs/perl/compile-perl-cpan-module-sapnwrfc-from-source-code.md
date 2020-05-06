---
metaTitle: "Perl - Compile Perl cpan module sapnwrfc from source code"
description: "Simple example to test the RFC connection"
---

# Compile Perl cpan module sapnwrfc from source code


I'd like to describe the prerequisites and the steps how to build the Perl CPAN module sapnwrfc with the Strawberry Perl environment under Windows 7 x64. It should work also for all later Windows versions like 8, 8.1 and 10.

I use Strawberry Perl 5.24.1.1 64 bit but it should also work with older versions.

It took me some hourse to succeed with several tries (32 vs. 64 bit installation of Perl, SAP NW RFC SDK, MinGW vs. Microsoft C compiler). So I hope some will benefit from my findings.



## Simple example to test the RFC connection


Simple example from [http://search.cpan.org/dist/sapnwrfc/sapnwrfc-cookbook.pod](http://search.cpan.org/dist/sapnwrfc/sapnwrfc-cookbook.pod)

```perl
use strict;
use warnings;
use utf8;
use sapnwrfc;

SAPNW::Rfc->load_config('sap.yml');
my $conn = SAPNW::Rfc->rfc_connect;

my $rd = $conn->function_lookup("RPY_PROGRAM_READ");
my $rc = $rd->create_function_call;
$rc->PROGRAM_NAME("SAPLGRFC");

eval {
$rc->invoke;
};
if ($@) {
    die "RFC Error: $@\n";
}

print "Program name: ".$rc->PROG_INF->{'PROGNAME'}."\n";
my $cnt_lines_with_text = scalar grep(/LGRFCUXX/, map { $_->{LINE} } @{$rc->SOURCE_EXTENDED});
$conn->disconnect;

```



#### Remarks


Install a current Strawberry Perl 64 bit package from [http://strawberryperl.com](http://strawberryperl.com). In my case it was 5.24.1.1.

Download the current version of the SAP NW RFC SDK x64 bit from [https://launchpad.support.sap.com/#/softwarecenter](https://launchpad.support.sap.com/#/softwarecenter)

You can find it with the following trace:
**Support Packages and Patches => By Category => Additional Components => SAP NW RFC SDK => SAP NW RFC SDK 7.20**

In my case the current version was 7.20 PL42 x64.

Extract the downloaded file with
`sapcar -xvf NWRFC_42-20004568.SAR`

I renamed the folder to `C:\nwrfcsdk_x64`

Create .def and .a files for the MinGW compiler / linker with the following commands in the directory C:\nwrfcsdk_x64:

```perl
gendef *.dll
dlltool --dllname icuin34.dll --def icuin34.def --output-lib icuin34.a
dlltool --dllname icudt34.dll --def icudt34.def --output-lib icudt34.a
dlltool --dllname icuuc34.dll --def icuuc34.def --output-lib icuuc34.a
dlltool --dllname libsapucum.dll --def libsapucum.def --output-lib libsapucum.a
dlltool --dllname libicudecnumber.dll --def libicudecnumber.def --output-lib libicudecnumber.a
dlltool --dllname sapnwrfc.dll --def sapnwrfc.def --output-lib sapnwrfc.a

```

In the dircectory C:\nwrfcsdk_x64\lib the following files should exist:

```perl
icudt34.a          
icudt34.def        
icudt34.dll        
icuin34.a          
icuin34.def        
icuin34.dll        
icuuc34.a          
icuuc34.def        
icuuc34.dll        
libicudecnumber.a  
libicudecnumber.def
libicudecnumber.dll
libsapucum.a       
libsapucum.def     
libsapucum.dll     
libsapucum.lib     
sapdecfICUlib.lib  
sapnwrfc.a         
sapnwrfc.def       
sapnwrfc.dll       
sapnwrfc.lib       

```

Start command prompt with `cmd.exe` and start the program `cpan`.

Start the command `get sapnwrfc` to download the Perl module sapnwrfc from CPAN.

Leave the cpan environment with the `exit` command.
Change directory to `C:\Strawberry\cpan\build\sapnwrfc-0.37-0`.

Build the Makefile(s) with the following command. Adapt the folder names according to your setup.

```perl
perl Makefile.PL --source=C:\nwrfcsdk_x64 --addlibs "C:\nwrfcsdk_x64\lib\sapnwrfc.a C:\nwrfcsdk_x64\lib\libsapucum.a"

```

Run the commands `dmake` and `dmake install` to build and install the module.

Copy the files from `C:\nwrfcsdk_x64\lib` to `C:\Strawberry\perl\site\lib\auto\SAPNW\Connection`.

