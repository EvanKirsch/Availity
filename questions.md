We highly recommend that you use one of the free source code management platforms (GitHub, GitLab or BitBucket) when storing your code. Once you are ready for us to look at your answers, send us the link to your code. If you have any questions about the homework, please do not hesitate to ask. Questions:

---
1. **Tell me about your proudest professional achievement. It can also be a personal or school project.**
    - [This BlueZone Script](https://github.com/EvanKirsch/Scripts/blob/master/awake.js)

This is the first code I ever wrote that ran in 'Prod' (my machine). That is why I'm so proud of it. Its not complex or frankly even good, but its the first code I ever wrote while being a 'Professional'. Looking back on it and thinking about everything before this point and after this point I have learned is what drives me to continue learning.
The script prevents your session for logging out due to inactivity by measuring your cursor position on the screen and comparing it to the previous positions. If the position hasn't changed then the script will enter inputs to keep the session alive.

2. **Tell me about something you have read recently that you would recommend and why. (Can be a Github Repo, Article, Blog, Book, etc)**
    - [The Suckless Philosophy](https://suckless.org/philosophy/)
    > "We strive to maintain minimalism and clarity to drive development to completion."
 
 While the suckless philosophy can be extreme and is unsuitable for many aspects of enterprise software development; its core idea: that usability comes from simplicity is a core value I hold while developing software. I am much more of a quality before quantity developer.

3. **How would you explain to your grandmother what Availity does?**
    - Availity helps the health care provider and the insurance company communicate 

Coding exercises:
---
1. You are tasked to write a checker that validates the parentheses of a LISP code. Write
a program (in Java or JavaScript) which takes in a string as an input and returns true if all the
parentheses in the string are properly closed and nested.
2. Availity receives enrollment files from various benefits management and enrollment
solutions (I.e. HR platforms, payroll platforms).  Most of these files are typically in EDI format.  However,
there are some files in CSV format.  For the files in CSV format, write a program in a language that makes
sense to you that will read the content of the file and separate enrollees by insurance company in its own
file. Additionally, sort the contents of each file by last and first name (ascending).  Lastly, if there are
duplicate User Ids for the same Insurance Company, then only the record with the highest version should
be included. The following data points are included in the file:
- User Id (string)
- First Name (string)
- Last Name (string)
- Version (integer)
- Insurance Company (string)