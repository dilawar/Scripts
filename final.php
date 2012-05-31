<?php include('header.php') ?>

<?php
session_start();
include('error.php');
include('sql_func.php');
include('print.php');

$sendmail= $_SESSION['HOME']."/send_mail.php";
include($sendmail);

$init = $_SESSION['init'];
$base_url = "http://".$init['base_url'];
$this_sem = $_SESSION['sem'];

echo printWarning("Your preferences have been recodred successfully.");
echo "<br><br>";
$details = getStudentInformation($this_sem);
if(!$details)
{
	echo printErrorSevere("I am not able to fetch your details right now.. Redirecting in 5 sec ..");
	header("Refresh: 5, url=$base_url./eeta.php");
}
else {
	$msg = "<html> <body>";
	$msg .= "<h3>Your personal details. <h3>";
	$msg .= printStudentInfo($details);
}
$history = getHistory($this_sem, 2);
$prevSem = getPreviousSem($this_sem, 1);
$pprevSem = getPreviousSem($this_sem, 2);
$pCourse = $history[0];
$ppCourse = $history[1];
$course1 = getCourseNameFaculty($pCourse['course_id']);
$course2 = getCourseNameFaculty($ppCourse['course_id']);

$msg .= "<h3> Previous two semester record </h3>";
$msg .= "<table border=\"1\">";
$msg .= "<tr> <td>".printSem($prevSem);
$msg .= "</td> <td> <b>".printCourse($course1);
$msg .= "</b> </td> </tr>";
$msg .= "<tr> <td>".printSem($pprevSem); 
$msg .= "</td> <td> <b>".printCourse($course2); 
$msg .= "</b> </td> </tr> </table><br> <br>";


$db = "ta".$this_sem;
$init = $_SESSION['init'];
$ldap = $_SESSION['ldap'];

$prefer = getPreferences($this_sem);

$msg .= "<h3> Your preferences for this semester </h3>";
$msg .= printPreference($prefer);

echo $msg;

$ldap = $_SESSION['ldap'];
$mailto = $ldap."@iitb.ac.in";
$msg .= "<font color='red'> In case of discrepencies, please visit the application interface to update your details.</font>";
$msg .= "<br> <br> Application link : www.".$_SESSION['base_url']."/eeta.php";

$msg .= "</body></html>";
if(sendEmail($mailto, $msg))
{
	echo "Mail sent successfully.";
}
else 
{
	echo "Problem sending email.";
}

?>
