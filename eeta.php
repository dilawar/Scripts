<?php 
session_save_path(getenv('HOME'."/sessions"));
session_start();

$HOME="/pg/rs/dilawar";
$_SESSION['HOME'] = $HOME;
session_save_path($HOME."/sessions/");
if(session_start())
{
}
else {
	echo "Problem loading session.";
}

$inifile = "$HOME"."/sessions/eeta.ini";
if(!file_exists($inifile)) {
	printErrorSevere("Init file does not exists.");
	header("No configuration file found. Application incomplete ..");
}
$conf = parse_ini_file($inifile);
if(!$conf)
{
	header("No configuration file found. Application incomplete ..");
}
else {
	$_SESSION['init'] = $conf;
}

?>

<h1>EE IITB Teaching Assistant Interface</h1>
<font size="3" color="black">
Please fill in your LDAP details. </font><br>
<font size="2" color="red">Note that we do not save your password. This is to authenticate you with proxy-server. </font>
<br>

<form action="authenticate.php" method="post">
Proxy Username : <input type="text" name="username" id="username" />
Proxy Password : <input type="password" size="25" name="pass" id="pass"> <br><br>
Semester : <select name="year"> 
	<option value="2012">2012</option>
	<option value="2011">2011</option>
	<option value="2010">2010</option>
</select> 
<select name="sem">
	<option value="Odd">Odd</option>
	<option value="Even">Even</option>
	<option value="Summer">Summer</option>
</select>
<!-- <input type="radio" name="Semster" value="Odd" checked="checked" >Odd
<input type="radio" name="Semester" value="Even" >Even
<input type="radio" name="Semester" value="Summer" >Summer<br /> -->
<br>
<br>
<input type="radio" name="Role" value="Student" checked="checked" />Student
<input type="radio" name="Role" value="Teacher" />Teacher<br />
<input type="submit" name="Submit" value="Submit" />
</form>


