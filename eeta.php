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
Semester : <select name="sem">
	<option value="2012-13-1">Sem 1, 2012-13</option>
	<option value="2011-12-2">Sem 2, 2011-12</option>
	<option value="2011-12-1">Sem 1, 2011-12</option>
</select>
<input type="submit" name="Submit" value="Submit" />
</form>
</body>
</html>
