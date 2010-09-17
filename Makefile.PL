use ExtUtils::MakeMaker;

my $file = "plugins/CustomHeader/config.yaml";
my $version = LoadFile($file)->{version} ||
    die "No version found in $file";

WriteMakefile(
    NAME            => 'Custom Header',
    VERSION         => $version,
    DISTNAME        => 'CustomHeader',
);
