#!/usr/bin/perl -w
use XML::XPath;
use XML::XPath::XMLParser;

my %defs = ();

my( $api_file, $source_file ) = @ARGV;

my $xpath = XML::XPath->new(filename => $source_file);
my $apixpath = XML::XPath->new(filename => $api_file);

my @nodes = $xpath->findnodes("/metadata/attr");
NODE: foreach $node ( @nodes ) {
    my $metadatatype = $xpath->findvalue("\@name", $node);
    my $metadatavariant = $xpath->findvalue("text()", $node);
    if($metadatatype eq "pass_as") {
	if($metadatavariant eq "ref") { $paramvariant = "(inout)"; }
	elsif($metadatavariant eq "out") { $paramvariant = "(output)"; }
	else { print STDERR "Unrecognized pass_as type: $metadatavariant\n"; 
	       next NODE; }
    } elsif($metadatatype eq "array") {
	$paramvariant = "(array foo)";
    } elsif($metadatatype eq "null-ok" || $metadatatype eq "null_ok") {
	$paramvariant = "(null-ok)";
    } elsif($metadatatype eq "type") {
	$paramvariant = "";
    } else {
	next NODE;
    }

    my $path = $xpath->findvalue("\@path", $node);
    my $mname = $apixpath->findvalue("$path [position()=last()]/../../\@cname");    my $oname = $apixpath->findvalue("$path [position()=last()]/../../../\@cname");
    $oname =~ s/GtkIcon_/Gtk/ ;
    next NODE if (!defined $mname);
    next NODE if ($mname eq "");
    next NODE if (!defined $oname || $oname eq "");

    if (!defined($defs{$mname})) {
	$defs{$mname} = "  (of-object $oname)\n  (parameters\n";
    }
    my @parnodes = $apixpath->findnodes($path);
    if( $#parnodes < 0) { 
	print STDERR "No parameters for $path\n";
	next NODE;
    }

#    my $sep = "'";
    foreach my $par ( @parnodes ) {
	my $name = $apixpath->findvalue("\@name", $par);
	my $type = $apixpath->findvalue("\@type", $par);
#	print "   $sep(\"$type\" \"$name\" ($passas))\n";
#	if($sep eq "'") { $sep = " "; }
	$defs{$mname} .= "    (\"$type\" \"$name\" $paramvariant)\n";
    }

}

@keys = sort(keys %defs);
foreach $mname (@keys) {
    print "(override-parameters $mname\n";
#    print "  (of-object $oname)\n";
    print $defs{$mname};
    print "  )\n)\n";
}

