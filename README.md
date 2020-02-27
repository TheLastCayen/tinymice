Tinymice is a simple and light Auto Clicker developed with Lazarus.  The target operating system is linux but windows user can compile the software by removing codebotctrls from the dependency.
This software need Sqlite. To avoid libsqlite3.so error, you need to install libsqlite3-dev.

**License**:

	GPL V3

**Screenshot**:

![](pictures/screenshot/Main.png)

![](pictures/screenshot/Options.png)

![](pictures/screenshot/systray.png)

**Linux dependency:**

###### Compile:

	Lazarus 2.0.6
	FPC 3.0.4
	Cross.Codebot 
	libxtst-dev
	libgl-dev

###### Execute:

	libsqlite3-dev

###### Compile command (Tested under Linux Mint 19.3):

```bash
'/usr/bin/fpc'  -MObjFPC -Scghi -CX -Cg -Os4 -XX -l -vewnhibq -Filib/x86_64-linux -FuAbout -Fulib/Cross.Codebot-master/source/lib/x86_64-linux -Fu../../.lazarus/lib/LazMouseAndKeyInput/lib/x86_64-linux -Fu../../Downloads/lazarus/Cross.Codebot-master/source/lib/x86_64-linux -Fu/usr/share/lazarus/2.0.6/lcl/units/x86_64-linux/gtk2 -Fu/usr/share/lazarus/2.0.6/lcl/units/x86_64-linux -Fu/usr/share/lazarus/2.0.6/components/lazutils/lib/x86_64-linux -Fu/usr/share/lazarus/2.0.6/packager/units/x86_64-linux -Fu. -FUlib/x86_64-linux -FEbin -obin/tinymice -dUseCThreads -dLCL -dLCLgtk2
```

**Ubuntu  stand dependency:**

```bash
sudo apt-get install libsqlite3-dev
```

**External Download:**

```bash
Lazarus && FPC: https://sourceforge.net/projects/lazarus/files/
Cross.Codebot:	https://github.com/sysrpl/Cross.Codebot
Sqlite:	https://www.sqlite.org/download.html
```

