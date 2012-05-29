<h1>EE IITB Teaching Assistant Interface</h1>
<?php
session_save_path(getenv('HOME'."/sessions"));
session_start();
include('error.php');
include('print.php');
$_SESSION['completeInfo'] = "yes";
?>

Thanks for submitting your details. 

<?php 
echo printStudentInformation($_POST);

session_write_close();
if($_SESSION['completeInfo'] == "no") {
	echo printErrorSevere("Incomplete information! Redirecting in 3 sec...");
	header("Refresh: 3, url=$base_url./get_info.php");
}
else {
	echo "<br> Details look complete.";
}
?>
<!-- Compose a hidden form for posting purpose only -->
<html>
<body>
<form action="database.php" method="post">
<input type="hidden" name="roll" value=<?php echo $_POST['roll'] ?> readonly>
<input type="hidden" name="ldap" value=<?php echo $_SESSION['user_ldap']; ?> readonly> 
<input type="hidden" name="specialization" value=<?php echo $_POST['specialization'] ?> readonly>
<input type="hidden" name="program" value=<?php echo $_POST['program'] ?> readonly>
<input type="hidden" name="category" value=<?php echo $_POST['category'] ?> readonly>
<input type="hidden" name="gradYear" value=<?php echo $_POST['gradYear'] ?> readonly>
<input type="hidden" name="gradSem" value=<?php echo $_POST['gradSem'] ?> readonly>
<input label="Submit" type="submit" name="Submit" value="Submit" >
</body>
</html>

