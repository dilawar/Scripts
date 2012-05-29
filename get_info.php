<h1> EE IITB Teaching Assitant Interface</h1>

<font color="red"> Your details are either incomplete or missing.</font>


<?php
session_start();
?>

<html>
<head>
<h3> Please fill this form </h3>
<style>
</style>
</head>
<body>
<font size="2" color="red"> All fields are mandatory.</font>
<br> <br>

<form action="submission.php" method="post">
Roll No :
<input type="text" name="roll" >

LDAP : <input type="text" name="ldap" value=<?php echo $_SESSION['user_ldap']; ?> readonly> 
<br>
<br>

Specialization : 
<select name="specialization"> 
	<option value="xx"></option>
	<option value="ee1">EE1 : Communication</option>
	<option value="ee2">EE2 : Control And Computing</option>
	<option value="ee3">EE3 : Power Electronics and Systems.</option>
	<option value="ee4">EE4 : Microelectronics and VLSI</option>
	<option value="ee5">EE4 : Electronic Systems</option>
</select> 
<br>
<br>
Program : 
<select name="program"> 
	<option value="xx"></option>
	<option value="rs">Reseach Scholar</option>
	<option value="mtech">M. Tech.</option>
	<option value="dd">Dual Degree (B.Tech++)</option>
</select> 

Category :
<select name="category">
	<option value="xx"></option>
	<option value="ta">TA</option>
	<option value="ira">Institute RA</option>
	<option value="pra">Project RA</option>
	<option value="sf">Self Financed</option>
	<option value="qip">QIP</option>
	<option value="sponsored">Sponsored</option>
</select>
<br> <br> <br>
<input label="Submit" type="submit" name="Submit" value="Submit" >
</form>
</body>
</html>
