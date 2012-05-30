<h1>EE IITB Teaching Assistant Interface</h1>
<?php
session_start();

include('error.php');
include('print.php');
include('func.php');
?>

<h3> Thanks for submitting your details. </h3>

<?php 

if(!checkStudentDetails($_POST))
{
	echo printErrorSevere("Details you hav provided do not look complete. Redirecting you to entry form in 3 seconds ...");
	echo printStudentInfo($_POST);
	$init = $_SESSION['init'];
	$base_url = $init['base_url'];
	$url = "http://".$base_url."/get_info.php";
	header("Refresh: 3, url=$url");
	exit;
}

else {
	echo printStudentInfo($_POST);
	echo "<br> Your Detail looks complete.";
}		

/* convert date to be sent to database. */
$info = $_POST;
$gradY = $info['gradYear'];
$gradS = $info['gradSem'];
if($gradS == "Odd") {
	$gradOn = "January 1 ".$gradY;
}
else {
	$gradOn = "August 1 ".$gradY;
}
$gradOn = date("Y-m-d", strtotime($gradOn));

?>
<!-- Compose a hidden form for posting purpose only -->
<html>
<body>
<form action="database.php" method="post">
<input type="hidden" name="roll" value=<?php echo $_POST['roll'] ?> readonly>
<input type="hidden" name="ldap" value=<?php echo $_SESSION['ldap']; ?> readonly> 
<input type="hidden" name="specialization" value=<?php echo $_POST['specialization'] ?> readonly>
<input type="hidden" name="program" value=<?php echo $_POST['program'] ?> readonly>
<input type="hidden" name="category" value=<?php echo $_POST['category'] ?> readonly>
<input type="hidden" name="graduatingOn" value=<?php echo $gradOn ?> readonly>
<input label="Submit" type="submit" name="Submit" value="Submit" >
</body>
</html>

