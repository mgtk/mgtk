#!/usr/bin/perl -w
use XML::XPath;
use XML::XPath::XMLParser;

my( $api_file, $source_file ) = @ARGV;

my $xpath = XML::XPath->new(filename => $source_file);
my $apixpath = XML::XPath->new(filename => $api_file);

my @nodes = $xpath->findnodes("/metadata/attr[\@name='pass_as']");
NODE: foreach $node ( @nodes ) {
    my $passas = $xpath->findvalue("text()", $node);
    if($passas eq "ref") { $passas = "inout"; }
    elsif($passas eq "out") { $passas = "output"; }
    else { print STDERR "Unrecognized pass_as type: $passas\n"; next NODE; }

    my $path = $xpath->findvalue("\@path", $node);
    $_ = $path;
    my $cname = $1 if ( /\@cname='([^']*)'/ );
    my $name = $1 if ( /\@name='([^']*)'/ );
    next NODE if (!defined $cname || !defined $name);

#    my $mname = $apixpath->findvalue("/api/namespace/*[(name()='object' or name()='boxed' or name()='interface') and \@cname='$cname']/method[\@name='$name']/\@cname");
    my $mname = $apixpath->findvalue("$path [position()=last()]/../../\@cname");
    my $oname = $apixpath->findvalue("$path [position()=last()]/../../../\@cname");
    $oname =~ s/GtkIcon_/Gtk/ ;
    next NODE if (!defined $mname);

    my @parnodes = $apixpath->findnodes($path);
    if( $#parnodes < 0) { 
	print STDERR "No parameters for $cname.$name\n";
	next NODE;
    }
    print "(override-parameters $mname\n";
    print "  (of-object $oname)\n";
    print "  (parameters\n";
    my $sep = "'";
    foreach my $par ( @parnodes ) {
	my $name = $apixpath->findvalue("\@name", $par);
	my $type = $apixpath->findvalue("\@type", $par);
	print "   $sep(\"$type\" \"$name\" ($passas))\n";
	if($sep eq "'") { $sep = " "; }
    }
    print "  )\n)\n";

}
