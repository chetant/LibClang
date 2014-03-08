#!/usr/bin/env perl

use File::Basename;
use Getopt::Std;

use warnings;
use strict;

sub usage()
{
    print STDERR << "EOF";
Usage: $0 -i INFILE -o OUTFILE
Will read and process 'INSERT_SRC <PATH>' tags in input file,
expand them with Cabal friendly wildcards.
EOF
exit;
}

my %opt;
getopts("hi:o:", \%opt) or usage();
usage() if($opt{"h"} || !exists($opt{"i"}) || $opt{"i"} eq '');

my $templateFile = $opt{"i"};
my $outFile = $templateFile;
$outFile =~ s/\.template$/.release/;
$outFile = $opt{"o"} if(exists($opt{"o"}));
warn "Reading $templateFile, writing to $outFile\n";
open(INF, $templateFile) or die "Cannot open $templateFile";
open(OUTF, '>', $outFile) or die "Cannot open $outFile to write";
while(<INF>)
{
    chomp;
    if(/^(\s*)--\s+\@INSERT_SRC\s+(.+)$/)
    {
	my $indent = $1;
	my $dirName = $2;

	my %dirMap;
	getExtraSrcEntries($dirName, \%dirMap);

	foreach my $dir (sort (keys %dirMap))
	{
	    foreach my $wcard (sort (keys %{$dirMap{$dir}}))
	    {
		print OUTF "$indent\"$dir/$wcard\"\n";
	    }
	}
    }
    else
    {
	print OUTF "$_\n";
    }
}
close(OUTF);
close(INF);

sub getWildCard
{
    my $fname = shift;

    if($fname =~ /[^\/]+?\.([^\/]+)$/)
    {
	return "*.$1";
    }
    else
    {
	return basename($fname);
    }
}

sub getExtraSrcEntries
{
    my ($path, $dirMap) = @_;
    warn "Scanning $path..\n";
    my $inf;
    open($inf, "find -L $path -type f |") or die("Cannot exec 'find $path -type f'");
    while(<$inf>)
    {
	chomp;
	${$dirMap}{dirname $_}{getWildCard $_} = 1;
    }
    close($inf);
}
