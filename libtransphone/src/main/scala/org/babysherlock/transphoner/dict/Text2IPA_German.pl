#!/usr/bin/perl -w

# Text2IPA_German.pl Version 0.9.1

# this assumes a UTF-8 file in one word per line format and 
# automatically transcribes German words in IPA
# usage:
# Text2IPA_German.pl file
# See help (-h) for options

use Getopt::Std;
use utf8;
binmode(STDOUT, ":utf8");
binmode(STDIN, ":utf8");

my $usage;
{
$usage = <<"_USAGE_";
This script attempts to generate an automatic phonetic transcription of German orthographic words 
using the International Phonetic Alphabet (IPA).

Notes and assumptions:
- The input text file is encoded in UTF-8 (without BOM)
- Each line of the input file contains exactly one word
- This script does NOT use a lexicon, meaning it will inevitably make 
  MANY mistakes - use it at your own risk! :)

Usage:  Text2IPA_German [options] <FILE>

Options and argument:

-h              print this message and quit
-s              Use this to [s]eparate syllables with '.'
-S              [S]tress: mark stressed syllables with a single quotation mark
-R <coda_r>     behavior of r in coda: choose between 'vowel' (default), 
                'semivowel' for open vowel with circumflex after a vowel or 
                'apical' for rolled r       
-r              use south German apical [r] instead of uvular R
-n              use syllabic [n]asals instead of schwa and nasal 
                (default: no syllabic nasals)
-j              mark syllable [j]oints with % before syllable border 
                (with this option <Matten> becomes [ma%.tn]

<FILE>    A text file encoded in UTF-8 without BOM, one word per line


Examples:

Read a file and output IPA to standard output:
  Text2IPA_German.pl in_utf8.txt

Read a file and output IPA to a file:
  Text2IPA_German.pl in_utf8.txt > out_utf8.txt

Use [S]tress marking and syllable [s]eparators: (possible to use separate dashes for each option)
  Text2IPA_German.pl -s -S in_utf8.txt > out_utf8.txt

Show syllable [j]oints and syllable [s]eparators: (all options after one dash)
  Text2IPA_German.pl -js in_utf8.txt > out_utf8.txt

Use 'semivowel' open [R] in coda:
  Text2IPA_German.pl -R semivowel in_utf8.txt > out_utf8.txt

Use apical [r] prevocally and in coda [R] (these options should usually be used together):
  Text2IPA_German.pl -r -R apical in_utf8.txt > out_utf8.txt

Copyright 2013, Amir Zeldes

This program is free software. You may copy or redistribute it under
the same terms as Perl itself.
_USAGE_
}

### OPTIONS BEGIN ###
%opts = ();
getopts('hnSjrsR:',\%opts) or die $usage;

#help
if ($opts{h} || (@ARGV == 0)) {
    print $usage;
    exit;
}

#syllabic nasals
$syl_nasals = 0;
if ($opts{n}) {$syl_nasals = 1;}

#stress marking
$stress = 0;
if ($opts{S}) {$stress = 1;}

#syllable separator
if ($opts{s}) {$syl_separator = ".";}
else {$syl_separator="";}

#syllable joint marker
if ($opts{j}) {$joint_marker = "%";}
else {$joint_marker="";}

#coda_r
if (!($coda_r = $opts{R})) 
    {$coda_r = "?";}
elsif($coda_r eq "apical") 
	{$coda_r="r";}
elsif($coda_r eq "semivowel") 
	{$coda_r="??";}
else {$coda_r = "?"}

#r_type
if ($opts{r}) {$r_type = "r";}
else{$r_type = "?";}
### OPTIONS END ###

open FILE,"<:encoding(UTF-8)",shift or die "could not find input document";
while (<FILE>) {

    chomp;
    $line = $_;

	$line = lc($line);
	
#qu > kv (qu stand for [kv], also written 'kw')
	$line =~ s/qu/kw/g;

#Find foreign morphemes (Identify foreign morphemes with non-German spelling, e.g. -tion in Information)
	$line =~ s/tion/zjon/g;

#Find syllables (Syllables are recognized using the onset maximization strategy - the longest possible onset 
#is preferred moving from each vowel to the left. Use a hyphen to manually input borders.)
	$line =~ s/ungs([^-])/ungs-$1/g; # join -s to syllable of compound stems in -ung
	$line =~ s/-/#/g;
	$line =~ s/([^s])ch/$1ç/g; # protect ch from being syllabified apart, e.g. herzlic-hen 
	$line =~ s/ck/K/g; # protect ck from being syllabified apart, e.g. gluec-kwunsch 
	$line =~ s/(((s+?(ch)?)?(pf|qu|[wvfbdgkKtpzç]|pp|tt|bb|dd|gg)?([lmnr]+)?|h|ß|z?j|zw|kw)((a|e|o)[iuh]?|i(eh?)?|u|y|ü|ö|ä))/#$1/g;
	$line =~ s/ç/ch/g;
	$line =~ s/K/ck/g;
	$line =~ s/(^.*$)/$1#/g;
	$line =~ s/##/#/g;


#Prohibit impossible syllables (imp)
	$line =~ s/#([^(s|ß|sch|m)])m([aeiouyüöä])/$1#m$2/g;
	$line =~ s/#z([lmnr][aeiouyüöä])/z#$1/g;

	
#Find prefixes affecting syllabification
	$line =~ s/^#ve#r/#ver-#/g;


#Find stress (Stress is assumed to be word initial, unless certain unstressed prefixes like ver- are found)
	$line =~ s/^([^aeiouyüöä]*)([aeiouyüöä])/$1\/$2/g;


#Identify unstressed verbal prefixes (Prefixes like ver- and be- are unstressed)
	$line =~ s/^#v\/er([^aeiouyüöä]*)([aeiouyüöä].*[aeiouyüöä])/#ver$1\/$2/g;
	$line =~ s/^#b\/e([^aeiouyüöä]*)([aeiouyüöä].*[aeiouyüöä])/#be$1\/$2/g;
	$line =~ s/^#\/ent([^aeiouyüöä]*)([aeiouyüöä].*[aeiouyüöä])/#ent$1\/$2/g;
	$line =~ s/^#g\/e([^aeiouyüöä]*)([aeiouyüöä].*[aeiouyüöä])/#ge$1\/$2/g;
	$line =~ s/^#z\/er([^aeiouyüöä]*)([aeiouyüöä].*[aeiouyüöä])/#zer$1\/$2/g;
	$line =~ s/^#\/er([^aeiouyüöä]*)([aeiouyüöä].*[aeiouyüöä])/#er$1\/$2/g;
	$line =~ s/^#\/em(#?)p([^aeiouyüöä]*)([aeiouyüöä].*[aeiouyüöä])/#em$1p$2\/$3/g;

#Identify stressed suffixes (e.g. -tion)
	$line =~ s/(.*)\/(.*)#zjon/$1$2#zj\/on/g;
	$line =~ s/(.*)\/(.*)ie\#r/$1$2\/ie\#r/g;

#Glottal stop (No initial and\/or stressed syllable (for some speakers no syllable) begins with a vowel - the vowel is preceded by a glottal stop by default)
	$line =~ s/\#(\/[aeiouyüöä])/\#?$1/g;
	$line =~ s/^\#([aeiouyüöä])/?$1/g;


#Identify sibilant [?] (The spelling sch and the initial clusters st- and sp- signal the sound [?])
	$line =~ s/sch/?/g;
	$line =~ s/#s([tp])/#?$1/g;


#consonants (Replace various consonants and digraphs with IPA symbols)
	$line =~ s/v/f/g;
	$line =~ s/w/v/g;
	$line =~ s/([aou]#?)ch/$1x/g;
	$line =~ s/ch/ç/g;
	$line =~ s/t?z/?/g;
	$line =~ s/tz/?/g;
	$line =~ s/ts/?/g;


#Voice s before vowel (s is voiced and pronounced [z] before vowels in syallble onset)
	$line =~ s/([^s]|^)s(\/?[aeiouyüöä])/$1z$2/g;


#Remove sharp s (ß is treated like \/s\/, but is not voiced before vowels in syallble onset)
	$line =~ s/ß/s/g;


#Final voice loss (Voiced obstruents become devoiced in the coda)
	$line =~ s/([^aeou])ig#/$1iç#/g;
	$line =~ s/b+([^aeiouyüöä]*)#/p$1#/g;
	$line =~ s/d+([^aeiouyüöä]*)#/t$1#/g;
	$line =~ s/g+([^aeiouyüöä]*)#/k$1#/g;
	$line =~ s/z+([^aeiouyüöä]*)#/s$1#/g;


#Identify vowels (Use orthographic clues (double consonants, the letter h etc.) to determine vowel quality and quantity)
	$line =~ s/([oua])h/$1?/g;
	$line =~ s/([^i]e)h/$1?/g;
	$line =~ s/([üy])h/y?/g;
	$line =~ s/([ö])h/ø?/g;
	$line =~ s/([^#])h/$1/g;
	$line =~ s/ei/\@?/g;
	$line =~ s/eu/??/g;
	$line =~ s/au/\@?/g;
	$line =~ s/ä/e/g;
	$line =~ s/i#(tt|pp|ck|tz|dd|bb|gg|ss|mm|nn|ll|rr)/?%#$1/g;
	$line =~ s/i(#?[^?aeiouyüöä#])([^aeiouyüöä#])/?$1$2/g;
	$line =~ s/i([lmnr]#)([ptkbdgç?])/?$1$2/g;
	$line =~ s/ie?([^?])/i?$1/g;
	$line =~ s/o#(tt|pp|ck|tz|dd|bb|gg|ss|mm|nn|ll|rr)/?%#$1/g;
	$line =~ s/o(#?[^?aeiouyüöä#])([^aeiouyüöä#])/?$1$2/g;
	$line =~ s/o([lmnr]#)([ptkbdgç?])/?$1$2/g;
	$line =~ s/o([^?])/o?$1/g;
	$line =~ s/u#(tt|pp|ck|tz|dd|bb|gg|ss|mm|nn|ll|rr)/?%#$1/g;
	$line =~ s/u(#?[^?aeiouyüöä#])([^aeiouyüöä#])/?$1$2/g;
	$line =~ s/u([lmnr]#)([ptkbdgç?])/?$1$2/g;
	$line =~ s/u([^?])/u?$1/g;
	$line =~ s/a#(tt|pp|ck|tz|dd|bb|gg|ss|mm|nn|ll|rr)/\@%#$1/g;
	$line =~ s/a(#?[^?aeiouyüöä#])([^aeiouyüöä#])/\@$1$2/g;
	$line =~ s/a([lmnr]#)([ptkbdgç?])/\@$1$2/g;
	$line =~ s/a([^?])/a?$1/g;
	$line =~ s/\@/a/g;
	$line =~ s/[yü]#(tt|pp|ck|tz|dd|bb|gg|ss|mm|nn|ll|rr)/?%#$1/g;
	$line =~ s/[yü](#?[^?aeiouyüöä#])([^aeiouyüöä#])/?$1$2/g;
	$line =~ s/[yü]([lmnr]#)([ptkbdgç?])/?$1$2/g;
	$line =~ s/[yü]([^?])/y?$1/g;
	$line =~ s/ö#(tt|pp|ck|tz|dd|bb|gg|ss|mm|nn|ll|rr)/œ%#$1/g;
	$line =~ s/ö(#?[^?aeiouyüöä#])([^aeiouyüöä#])/œ$1$2/g;
	$line =~ s/ö([lmnr]#)([ptkbdgç?])/œ$1$2/g;
	$line =~ s/ö([^?])/ø?$1/g;
	$line =~ s/e#(tt|pp|ck|tz|dd|bb|gg|ss|mm|nn|ll|rr)/?%#$1/g;
	$line =~ s/([^\/])e#/$1?#/g;
	$line =~ s/(.*\/.*[^\/])en#/$1N#/g;
	$line =~ s/(.*\/.*[^\/])em#/$1M#/g;
	$line =~ s/([^\/])el#/$1L#/g;
	$line =~ s/(.*\/.*)([^\/])er#/$1$2?#/g;
	$line =~ s/(\/.*[^\/])er(n?)#/$1?$2#/g;
	$line =~ s/([^\/])er#/$1?R#/g;
	$line =~ s/e([lnm])#/?$1#/g; # The condition that the syllable must be unstressed was removed - fixes <Ende> and ruins long <den>
	$line =~ s/e(#?[^?aeiouyüöäo?????œø#])([^aeiouyüöäo?????œøN#])/?$1$2/g;
	$line =~ s/e([^?])/e?$1/g;
	$line =~ s/([aeiouyüöäo?????œø]??)r+([^aeiouyüöäo?????œø]*)#/$1R$2#/g;
    $line =~ s/??/?R/g;
    $line =~ s/nN#/n?n#/g;	# This was added to avoid Auslaut in <-nen> being a syllable with \/n\/ Anlaut and syllabic N [nN]


#Identify short suffixes (-lich and -ig are assumed to be short suffixes)
	$line =~ s/(l?)i?(#?)ç/$1?$2ç/g;


#Engma (replace n with engma before velars)
	$line =~ s/n#([gk])/?#$1/g;
	$line =~ s/n[gk]#/?#/g;


#Double consonant orthography (Remove double consonant orthography while identifying related syllable joints)
	$line =~ s/gg/g/g;
	$line =~ s/bb/b/g;
	$line =~ s/dd/d/g;
	$line =~ s/[ck]k/k/g;
	$line =~ s/pp/p/g;
	$line =~ s/tt/t/g;
	$line =~ s/ss/s/g;
	$line =~ s/ll/l/g;
	$line =~ s/mm/m/g;
	$line =~ s/rr/r/g;
	$line =~ s/nn/n/g;

#Standard R (use standard German uvular [R])
	$line =~ s/r/$r_type/g;

#replace intermediate symbols
$line =~ s/%.%//g;
$line =~ s/a\//á/g;
$line =~ s/e\//é/g;
$line =~ s/i\//í/g;
$line =~ s/o\//ó/g;
$line =~ s/u\//ú/g;
$line =~ s/<\//&/g;
$line =~ s/\//?/g;
$line =~ s/&/<\//g;
$line =~ s/^//g;
$line =~ s/@/?/g;
$line =~ s/l\./?/g;
$line =~ s/r\./?/g;
$line =~ s/~//g;
$line =~ s/\%/$joint_marker/g;
$line =~ s/R/$coda_r/g;
$line =~ s/N/n?/g;
$line =~ s/M/m?/g;
$line =~ s/L/l?/g;

#schwa instead of syllabic consonants if desired
if ($syl_nasals == 0)
{
$line =~ s/n?/?n/g;
$line =~ s/m?/?m/g;
$line =~ s/l?/?l/g;
}


#stress mark if desired
if ($stress == 1)
{
$line =~ s/(^|\#)([^\#]*)?([^\#]*(\#|\$))/$1'$2$3/g;
}
else
{
$line =~ s/?//g;
}

#remove initial and final border
$line =~ s/(^#|#$)//g;

#choose syllable marker
$line =~ s/#/$syl_separator/g;

#add apical trill for pure open R syllables if apical is selected
if($coda_r eq "r") {$line =~ s/?/?r/g;}

print $line . "\n";

}