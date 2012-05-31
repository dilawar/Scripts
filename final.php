<?php include('header.php') ?>

<?php
session_start();
include('error.php');
include('func.php');
include('error.php');
include('sql_func.php');

$init = $_SESSION['init'];
$base_url = "http://".$init['base_url'];
$this_sem = $_SESSION['sem'];
$ldap = $_SESSION['ldap'];

echo printWarning("Your preferences have been recodred successfully.");
echo "<br><br>";
echo "<b> An email have been sent with your updated information. </b>";





?>
