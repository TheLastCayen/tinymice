Tinymice is a simple and light Auto Clicker developed with Lazarus.  The target operating system is linux but windows user can compile the software by removing codebotctrls from the dependency.
This software need Sqlite. To avoid libsqlite3.so error, you need to install libsqlite3-dev.

**License**:

	GPL V3

**Screenshot**:

![](pictures/screenshot/Main.png)

![](pictures/screenshot/Options.png)

![](pictures/screenshot/systray.png)

**Install Instructions(more packages will be add soon)**

###### Download

​	[Ubuntu](https://raw.githubusercontent.com/TheLastCayen/tinymice/master/bin/tinymice_0.5-1.deb)

###### Ubuntu Install Command

	sudo dpkg -i tinymice_0.5-1.deb

**Linux dependency:**

###### Compile:

	Lazarus 2.0.6
	FPC 3.0.4
	libxtst-dev
	libsqlite3-dev

###### Compile command:

```bash
lazbuild ptinymice.lpi
```

**External Download:**

```bash
Lazarus && FPC: https://sourceforge.net/projects/lazarus/files/
Cross.Codebot:	https://github.com/sysrpl/Cross.Codebot
Sqlite:	https://www.sqlite.org/download.html
```

