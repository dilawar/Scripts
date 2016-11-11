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
        fclose($opanak); //explicitly close open socket connection
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

    $sr = ldap_search($ds, $base_dn, "uid=$ldap");
    $info = ldap_get_entries($ds, $sr);

    $result = array();

    for( $s=0; $s < $info['count']; $s++)
    {
        $i = $info[$s];

        $laboffice = $i['profilelaboffice'][0];
        array_push($result
            , array(
                "fname" => $i['givenname'][0]
                , "lname" => $i['sn'][0]
                , "uid" => $i['profileidentification'][0]
                , "email" => $i['mail'][0]
                , "laboffice" => $laboffice
                //, "joined_on" => $i['profiletenureend'][0]
            )
        );
    }
    return $result[0];
}

print_r( getUserInfoFromLdap( $argv[1]  ));

?>
