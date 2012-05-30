<?php include('header.php'); ?>
<?php 

$HOME="/pg/rs/dilawar";
session_save_path(getenv('HOME'."/sessions"));
session_start();

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

function getSem($conf) 
{
	if($conf['sem'] == 1) {
		return "Odd"; 
	} 
	else { 
		return "Even"; 
	} 
}

?>

<font size="3" color="black">
Please fill in your <i>LDAP details </i>. </font><br>
<font size="2" color="red">Note that we do not save your password. This is to authenticate you with proxy-server. </font>
<br>
<br>
<form action="authenticate.php" method="post">
Username : <input type="text" name="username" id="username" />
Password : <input type="password" size="25" name="pass" id="pass"> <br><br>
Year : <input type="text" name="year" value=<?php echo $conf['year'] ?> readonly />
Semester : <input type="text" name="sem" value=<?php echo getSem($conf); ?> readonly />

<!-- <input type="radio" name="Semster" value="Odd" checked="checked" >Odd
<input type="radio" name="Semester" value="Even" >Even
<input type="radio" name="Semester" value="Summer" >Summer<br /> -->
<br>
<br>
<input type="radio" name="Role" value="Student" checked="checked" />Student
<input type="radio" name="Role" value="Teacher" />Teacher<br />
<input type="submit" name="Submit" value="Submit" />
</form>

