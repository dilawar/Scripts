<h1> EE IITB Teaching Assitant Interface</h1>

<?php
session_save_path(getenv('HOME'."/sessions"));
session_start();
## check if details are mission or incomplete.
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

LDAP : <input type="text" name="ldap" value=<?php echo $_SESSION['ldap']; ?> readonly> 
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
<br> <br> 
Graduation Date :
<select name="gradYear">
	<option value="2012">2012</option>
	<option value="2013">2013</option>
	<option value="2014">2014</option>
	<option value="2015">2015</option>
	<option value="2016">2016</option>
	<option value="2017">2017</option>
	<option value="2018">2018</option>
	<option value="2019">2019</option>
	<option value="2020">2020</option>
	<option value="2021">2021</option>
	<option value="2022">2022</option>
</select>
<select name="gradSem"> 
	<option value="even">Even Semester</option>
	<option value="odd">Odd Semester</option>
</select> 
<br>
<br>
<input label="Submit" type="submit" name="Submit" value="Submit" >
</form>
</body>
</html>
