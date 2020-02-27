TinyMice is a simple and light Auto Clicker developed with Lazarus.  

The target operating system is Linux, but windows user can compile the software by removing codebotctrls from the Project Dependency. There is also a version of Lazarus for Mac but since I don't own a mac, I can't try to port TinyMice. 

**License**:

	GPL V3

**Screenshot**:

![](pictures/screenshot/Main.png)

![](pictures/screenshot/Options.png)

![](pictures/screenshot/systray.png)

**Install Instructions**
(more packages coming soon)

###### Download

​	[Ubuntu](https://raw.githubusercontent.com/TheLastCayen/tinymice/master/bin/tinymice_0.5-1.deb)

###### Ubuntu Install Command

```bash
sudo dpkg -i tinymice_0.5-1.deb
```

**Compiling Dependency :**

###### Compiler && IDE: 

- Lazarus 2.0.6
- FPC 3.0.4

###### Linux Library: 

- Libxtst development package
- sqlite3 development package

###### Windows Library: 

- sqlite3

  

##### Installing Linux library:

- Ubuntu:  ```sudo apt-get install libxtst-dev libsqlite3-dev ```

- Fedora : ``` sudo yum ```

  

**Compile command:**

```bash
lazbuild ptinymice.lpi
```

**External Download Links:**

[Lazarus && FPC](https://sourceforge.net/projects/lazarus/files/)
[Cross.Codebo](https://github.com/sysrpl/Cross.Codebot)
[Sqlite](https://www.sqlite.org/download.html)


