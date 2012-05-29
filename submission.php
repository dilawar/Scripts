<?php
session_start();
include('error.php');
$_SESSION['completeInfo'] = "yes";
?>

Thanks for submitting your details. 
<br>
<br>
<b> Roll no : </b> : <?php echo $_POST['roll']; ?> <br>

<?php if(strlen($_POST['roll']) < 7) 
{
	$_SESSION['completeInfo'] = "no";
} 
?>

<b> Specialization : </b> 
<?php
$course = $_POST['specialization'];
if($course == "xx") {
	echo "Not Given";
	$_SESSION['completeInfo'] = "no";
}
else if($course == "ee1") {
	echo "Communication";
}
else if($course == "ee2") {
	echo "Control and Computing";
}
else if($course == "ee3") {
	echo "Power Electronics and System";
}
else if($course == "ee4") {
	echo "Microelectronics and VLSI";
}
else {
	echo "Electronic Systems";
} ?> <br>
<b> Program </b> 
<?php 
	$prog = $_POST['program'];
switch($prog) 
{
	case "xx" :
		echo "Not given";
		$_SESSION['completeInfo'] = "no";
		break;
	case "rs" :
		echo "Research Scholar";
		break;
	case "mtech" :
		echo "Master of Technology";
		break;
	case "dd" :
		echo "Dual Degree (B.Tech + M.Tech.)";
		break;
} ?> <br>

<b> Category : </b>

<?php 	
$cat = $_POST['category'];
switch($cat) 
{
	case "xx" :
		echo "Not given";
		$_SESSION['completeInfo'] = "no";
		break;
	case "ta" :
		echo "Teaching Assistant";
		break;
	case "ira" :
		echo "Institute Research Assistant";
		break;
	case "pra" :
		echo "Project Research Assistant";
		break;
	case "sf":
		echo "Self Financed";
		break;
	case "qip" :
		echo "Qualility Imporvement Program";
		break;
	case "sponsored" :
		echo "Sponsored";
		break;
} ?> <br>

<?php 
session_write_close();
if($_SESSION['completeInfo'] == "no") {
	echo printErrorSevere("Incomplete information! Redirecting in 3 sec...");
	header("Refresh: 3, url=$base_url./get_info.php");
}
else {
	echo "<br> Complete information.";
}
?>

<html>
<body>
<form action="database.php" method="post">
<input type="submit" name="Submit" value="Submit" />
</form>
</body>
</html>

