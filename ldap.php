#!/usr/bin/env php
<?php

function findGroup( $laboffice )
{
    if( strcasecmp( $laboffice, "faculty" ) == 0 )
        return "FACULTY";
    if( strcasecmp( $laboffice, "instem" ) == 0 )
        return "FACULTY";
    return $laboffice;
}

function serviceping($host, $port=389, $timeout=1)
{
    $op = fsockopen($host, $port, $errno, $errstr, $timeout);
    if (!$op) return 0; //DC is N/A
    else {
        fclose($op); //explicitly close open socket connection
        return 1; //DC is up & running, we can safely connect with ldap_connect
    }
}

function getUserInfoFromLdap( $ldap, $ldap_ip="ldap.ncbs.res.in" )
{
    $base_dn = 'dc=ncbs,dc=res,dc=in';
    if( 0 == serviceping( $ldap_ip, 389, 2 ) )
    {
        echo "Could not connect to LDAP server. Timeout ... ";
        return NULL;
    }

    $ds = ldap_connect($ldap_ip) or die( "Could not connect to $ldap_ip" );
    $r = ldap_bind($ds); // or die( "Can't bind to $ldap_ip" );
    if( ! $r )
    {
        echo printWarning( "LDAP binding failed. TODO: Ask user to edit details " );
        return Array( );
    }


    $justthese = ["ou", "sn", "givenname", "mail", "profilelaboffice"];
    $filter = "(|(uid=$ldap)(profilelaboffice=$ldap))";
    $sr = ldap_search($ds, $base_dn, $filter, $justthese);
    $info = ldap_get_entries($ds, $sr);
    return $info;

    $result = array();
    for( $s=0; $s < $info['count']; $s++)
    {
        $i = $info[$s];
        $result[] = array_values($i[0]);
    }
    return $result;
}

$res = getUserInfoFromLdap($argv[1]);
$i = 0;
foreach($res as $k => $v)
{
    $i += 1;
    foreach($v as $val)
        if(is_array($val))
            echo "$i, ", $val[0]. ", ";
    echo "\n";
}

?>
