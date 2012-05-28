<html>
<body>
<h1>EE IITB Teaching Assistant Interface</h1>
<h2>Authentication</h2>
Please fill in your LDAP details. <br>
Note that we do not save your password. This is to authenticate you with proxy-server. 
<br>
<br>
<form action="authenticate.php" method="post">
LDAP ID : <input type="text" name="username" id="username" />
Password : <input type="password" size="25" name="pass" id="pass">
<input type="submit" name="Submit" value="Submit" />
</form>
</body>
</html>
