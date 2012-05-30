<h1>EE IITB Teaching Assistant Interface</h1>
<?php
session_start();
include('error.php');
include('print.php');
include('func.php');
?>

Thanks for submitting your details. 

<?php 

if(!checkStudentDetails($_POST))
{
	printErrorSevere("Details are not complete.");
	echo printStudentInfo($_POST);
}

else {
	echo printStudentInfo($_POST);
	echo "<br> Details look complete.";
}
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
<input type="hidden" name="gradYear" value=<?php echo $_POST['gradYear'] ?> readonly>
<input type="hidden" name="gradSem" value=<?php echo $_POST['gradSem'] ?> readonly>
<input label="Submit" type="submit" name="Submit" value="Submit" >
</body>
</html>

