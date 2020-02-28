



# Description

TinyMice is a simple and light Auto Clicker developed with Lazarus. 

The target operating system is Linux, but Windows users can compile the software by removing codebotctrls from the project dependency. There is also a version of Lazarus for Mac OS, but I will need help to support a Mac OS binary.



# **License**

GNU General Public License [(GPL v3)](https://www.gnu.org/licenses/gpl-3.0.html)

![](https://www.gnu.org/graphics/gplv3-or-later.png)



# **Screenshot**

![](pictures/screenshot/Main.png)

![](pictures/screenshot/Options.png)

![](pictures/screenshot/systray.png)

# How To Install

### Download

​	[Ubuntu](https://raw.githubusercontent.com/TheLastCayen/tinymice/master/bin/tinymice_0.5-1.deb)

(more packages coming soon)

### Command

- Ubuntu:  ```sudo dpkg -i tinymice_0.5-1.deb ```
- Fedora : (Coming soon)



# How To Compile

### **Dependency**

##### Compiler && IDE: 

- Lazarus 2.0.6
- FPC 3.0.4

##### Linux Library: 

- Libxtst development package
- sqlite3 development package

##### Windows Library: 

- sqlite3

  

### Installing Library

- Ubuntu:  ```sudo apt-get install libxtst-dev libsqlite3-dev ```

- Fedora :  ```sudo yum install libXtst-devel.x86_64 libsqlite3x-devel.x86_64 ```

  

### Command

```bash
lazbuild ptinymice.lpi
```



# External Download Links

[Lazarus && FPC](https://sourceforge.net/projects/lazarus/files/)

[Cross.Codebo](https://github.com/sysrpl/Cross.Codebot)

[Sqlite](https://www.sqlite.org/download.html)


