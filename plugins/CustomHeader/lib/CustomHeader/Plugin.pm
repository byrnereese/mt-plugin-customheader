package CustomHeader::Plugin;

use strict;
use Symbol;
use MT::Util qw( epoch2ts encode_url format_ts relative_date );

sub ERROR ()     { 0 }
sub SUCCESS ()   { 1 }
sub OVERWRITE () { 2 }

use Exporter;
*import = \&Exporter::import;
use vars qw( @EXPORT_OK %EXPORT_TAGS);
@EXPORT_OK = qw( ERROR SUCCESS OVERWRITE );
%EXPORT_TAGS = (constants => [ qw(ERROR SUCCESS OVERWRITE) ]);

sub plugin {
    return MT->component('CustomHeader');
}

sub uses_custom_header {
    my $app = MT::App->instance;
    my $blog = $app->blog;
    return 0 unless $blog;

    # If the user has forcibly enabled custom header, then return true.
    #return 1 if plugin()->get_config_value('force_enable_custom_header','blog:'.$blog->id);
 
    # If the user is utilizing a template set for which custom css has been enabled
    # for an index template, return true.
    my $ts = $blog->template_set;
    return 0 unless $ts;
    return 1 if defined($app->registry('template_sets')->{$ts}->{'custom_header'});
    return 0;

}

sub custom_header {
    my $app = shift;
    return $app->load_tmpl( 'custom_header.tmpl' );
}

sub _crop_filename {
    my $asset   = shift;
    my (%param) = @_;
    my $file    = $asset->file_name or return;

    require MT::Util;
    my $format = $param{Format} || MT->translate('%f-cropped-%X.%Y-%wx%h%x');
    my $width  = $param{Width}  || 'auto';
    my $height = $param{Height} || 'auto';
    my $X      = $param{X} || '0';
    my $Y      = $param{Y} || '0';
    $file =~ s/\.\w+$//;
    my $base = File::Basename::basename($file);
    my $id   = $asset->id;
    my $ext  = lc($param{Type}) || $asset->file_ext || '';
    $ext = '.' . $ext;
    $format =~ s/%w/$width/g;
    $format =~ s/%h/$height/g;
    $format =~ s/%f/$base/g;
    $format =~ s/%i/$id/g;
    $format =~ s/%X/$X/g;
    $format =~ s/%Y/$Y/g;
    $format =~ s/%x/$ext/g;
    return $format;
}

sub custom_header_crop {
    my $app = shift;

    my $q = $app->param;
    my $blog = $app->blog;

    my $fmgr;
    my $result;

    my $X      = $q->param('x');
    my $Y      = $q->param('y');
    my $width  = $q->param('w');
    my $height = $q->param('h');
    my $id     = $q->param('id');

    my $asset = MT->model('asset')->load( $id );
    my $cropped = _crop_filename( $asset,
                  Width => max_width(),
                  Height => max_height(),
                  X => $X,
                  Y => $Y,
    );
    my $cache_path; my $cache_url;
    my $archivepath = $blog->archive_path;
    my $archiveurl  = $blog->archive_url;
    $cache_path = $cache_url = $asset->_make_cache_path( undef, 1 );
    $cache_path =~ s!%a!$archivepath!;
    $cache_url =~ s!%a!$archiveurl!;
    my $cropped_path = File::Spec->catfile( $cache_path, $cropped );
    my $cropped_url = $cache_url . '/' . $cropped;
    my ( $base, $path, $ext ) =
    File::Basename::fileparse( $cropped, qr/[A-Za-z0-9]+$/ );
    my $asset_cropped = new MT::Asset::Image;
    $asset_cropped->blog_id($blog->id);
    $asset_cropped->url($cropped_url);
    $asset_cropped->file_path($cropped_path);
    $asset_cropped->file_name("$base$ext");
    $asset_cropped->file_ext($ext);
    $asset_cropped->image_width(max_width());
    $asset_cropped->image_height(max_height());
    $asset_cropped->created_by( $app->user->id );
    $asset_cropped->label($app->translate("Cropped header for [_1]", $asset->label || $asset->file_name));
    $asset_cropped->parent( $asset->id );
    $asset_cropped->save;

    custom_header_id($blog, $asset_cropped->id);

    require MT::Image;
    my $img = MT::Image->new( Filename => $asset->file_path )
        or MT->log({message => "Error loading image: " . MT::Image->errstr });
    my $data = _crop($img,
             Width  => $width,
             Height => $height,
             X      => $X,
             Y      => $Y,
    );
    $data = $img->scale(
        Width  => max_width(),
        Height => max_height(),
    );

    require MT::FileMgr;
    $fmgr ||= $blog ? $blog->file_mgr : MT::FileMgr->new('Local');
    return undef unless $fmgr;
    return undef unless $fmgr->can_write($cache_path);

    my $error = '';
    if (!-d $cache_path) {
        require MT::FileMgr;
        my $fmgr = $blog ? $blog->file_mgr : MT::FileMgr->new('Local');
        $fmgr->mkpath($cache_path) or return undef;
    }

    $fmgr->put_data( $data, $cropped_path, 'upload' )
        or $error = MT->translate( "Error creating cropped file: [_1]", $fmgr->errstr );

    $result = {
        error        => $error,
        cropped      => $cropped,
        cropped_path => $cropped_path,
        cropped_url  => $cropped_url,
    };

    return _send_json_response($app, $result);
}

sub _send_json_response {
    my ($app,$result) = @_;
    require JSON;
    my $json = JSON::objToJson( $result );
    $app->send_http_header("");
    $app->print($json);
    return $app->{no_print_body} = 1;
    return undef;
}

sub _crop {
    my $image = shift;
    my %param = @_;
    my ($w, $h, $x, $y) = @param{qw( Width Height X Y )};
    my $magick = $image->{magick};
    my $err = $magick->Crop(
        'width' => $w,
        'height' => $h,
        'x' => $x,
        'y' => $y,
    );
    return $image->error(
    MT->translate(
        "Cropping a [_1]x[_2] image at [_3],[_4] failed: [_5]",
        $w, $h, $x, $y, $err)) if $err;

    ## Remove page offsets from the original image, per this thread:
    ## http://studio.imagemagick.org/pipermail/magick-users/2003-September/010803.html
    $magick->Set( page => '+0+0' );
    ($image->{width}, $image->{height}) = ($w, $h);
    wantarray ? ($magick->ImageToBlob, $w, $h) : $magick->ImageToBlob;
}

sub custom_header_id {
    my $blog = shift;

    my $app;
    if ( !$blog ) {
        $app = MT->instance;
        $blog = $app->blog if $app and $app->can('blog');
        if ( !$blog ) {
            my $msg = 'No blog in custom_header_id with app '.$app."\n";
            Carp::confess($msg);
            die;            
        }
    }
    # return unless $blog;

    my $key = 'CustomHeader:' . $blog->id;
    my $data;
    $data = MT->model('plugindata')->load({ plugin => 'CustomHeader',
                                            key    => $key });
    if (@_) {
        if ($data && $_[0] == 0) {
            $data->remove or die $data->errstr;
        } else {
            $data = MT::PluginData->new unless $data;
            $data->plugin('CustomHeader');
            $data->key($key);
            $data->data({ 'custom_header' => $_[0] });
            $data->save or die $data->errstr;
        }
    }
    return unless $data;
    my $id = $data->data->{'custom_header'};
    return $id if ($id && MT->model('asset')->exist( $id ));
    return undef;
}

sub custom_header_upload {
    my $app = shift;
    my $result = _upload_file( $app,
        require_type => ('image'),
        @_,
    );
    if (    $result->{status} == SUCCESS()
        &&  ($result->{asset}->{width} < max_width()
        ||  $result->{asset}->{height} < max_height())) {
        require MT::Asset;
        my $a = MT::Asset->load($result->{asset}->{id});
        $a->remove();
        $result = {
            status => ERROR(),
            message => 'The image you uploaded is too small. Please make sure it is at least ' .
            max_width() . ' pixels wide and ' . max_height() . ' pixels high.',
        };
    }
    return _send_json_response($app, $result);
}

sub custom_header_reset {
    my $app = shift;
    custom_header_id($app->blog, 0);
    return _send_json_response($app, { status => SUCCESS() });
}

sub tag_asset_parent {
    my ($ctx, $args, $cond) = @_;
    my $a = $ctx->stash('asset');
    my $p = MT::Asset->load($a->parent);
    return if !$p;
    local $ctx->{__stash}->{asset} = $p;
    defined(my $out = $ctx->slurp($args,$cond)) or return;
    return $out;
}

sub tag_has_header {
    my ($ctx, $args, $cond) = @_;
    my $id = custom_header_id( $ctx->stash('blog') );
    return MT->model('asset')->exist( { id => $id } );
}

sub tag_custom_header {
    my ($ctx, $args, $cond) = @_;
    require MT::Asset::Image;
    my $id = custom_header_id( $ctx->stash('blog') );
    return undef if ($id eq '');
    my $a = MT::Asset::Image->load($id);
    local $ctx->{__stash}->{asset} = $a;
    defined(my $out = $ctx->slurp($args,$cond)) or return;
    return $out;
}
sub tag_custom_header_width {
    my ($ctx, $args, $cond) = @_;
    return max_width( $ctx->stash('blog') );
}
sub tag_custom_header_height {
    my ($ctx, $args, $cond) = @_;
    return max_height( $ctx->stash('blog') );
}
sub _max_dim {
    my ($args) = @_;
    my $app = MT->instance;
    my $blog = ref $args->{blog} ? $args->{blog}
             : $app->can('blog') ? $app->blog
                                 : undef;
    defined $blog or Carp::confess('CustomHeader::Plugin::_max_dim could not '
                                  .'find blog from environment. ');
    my $r  = $app->registry('template_sets');
    my $ts = $blog->template_set;
    if (my $hdr = $r->{$ts}->{'custom_header'}) {
        return $hdr->{$args->{dim}};
    }
    return undef;
}

sub max_width {
    return _max_dim({ blog => $_[0], dim => 'max_width' });
}

sub max_height {
    return _max_dim({ blog => $_[0], dim => 'max_height' });
}

sub _upload_file {
    my $app = shift;
    my (%upload_param) = @_;

    if (my $perms = $app->permissions) {
        return { status => ERROR(),
                 message => $app->translate("Permission denied."),
            } unless $perms->can_upload;
    }

    $app->validate_magic()
        or return { status => ERROR(),
                    message => $app->translate("Failed to validate magic.")
                };

    my $q = $app->param;
    my ($fh, $info) = $app->upload_info('file');
    my $mimetype;
    if ($info) {
        $mimetype = $info->{'Content-Type'};
    }
    my $has_overwrite = $q->param('overwrite_yes') || $q->param('overwrite_no');
    my %param = (
        entry_insert => $q->param('entry_insert'),
        middle_path  => $q->param('middle_path'),
        edit_field   => $q->param('edit_field'),
        site_path    => $q->param('site_path'),
        extra_path   => $q->param('extra_path'),
        upload_mode  => $app->mode,
    );
    return {    status => ERROR(),
                message => $app->translate("Please select a file to upload."),
            } if !$fh && !$has_overwrite;
    my $basename = $q->param('file') || $q->param('fname');
    $basename =~ s!\\!/!g;    ## Change backslashes to forward slashes
    $basename =~ s!^.*/!!;    ## Get rid of full directory paths
    if ( $basename =~ m!\.\.|\0|\|! ) {
        return { status => ERROR(),
                 message => $app->translate( "Invalid filename '[_1]'", $basename ),
                };
    }
    if (my $asset_type = $upload_param{require_type}) {
        require MT::Asset;
        my $asset_pkg = MT::Asset->handler_for_file($basename);

        my %settings_for = (
            audio => {
                class => 'MT::Asset::Audio',
                error => $app->translate( "Please select an audio file to upload." ),
            },
            image => {
                class => 'MT::Asset::Image',
                error => $app->translate( "Please select an image to upload." ),
            },
            video => {
                class => 'MT::Asset::Video',
                error => $app->translate( "Please select a video to upload." ),
            },
        );

        if (my $settings = $settings_for{$asset_type}) {
            return { status => ERROR(), message => $settings->{error} }
                if !$asset_pkg->isa($settings->{class});
        }
    }

    my ($blog_id, $blog, $fmgr, $local_file, $asset_file, $base_url,
      $asset_base_url, $relative_url, $relative_path);
    if ($blog_id = $q->param('blog_id')) {
        $param{blog_id} = $blog_id;
        require MT::Blog;
        $blog = MT::Blog->load($blog_id)
            or return { status => ERROR(),
            message => $app->translate('Can\'t load blog #[_1].', $blog_id) };
        $fmgr = $blog->file_mgr;

        ## Set up the full path to the local file; this path could start
        ## at either the Local Site Path or Local Archive Path, and could
        ## include an extra directory or two in the middle.
        my ( $root_path, $middle_path );
        if ( $q->param('site_path') ) {
            $root_path = $blog->site_path;
        }
        else {
            $root_path = $blog->archive_path;
        }
        return { status => ERROR(),
                 message => $app->translate("Before you can upload a file, you need to publish your blog.")
            } unless -d $root_path;
        $relative_path = $q->param('extra_path');
        $middle_path = $q->param('middle_path') || '';

        my $relative_path_save = $relative_path;
        if ( $middle_path ne '' ) {
            $relative_path =
              $middle_path . ( $relative_path ? '/' . $relative_path : '' );
        }
        my $path = $root_path;
        if ($relative_path) {
            if ( $relative_path =~ m!\.\.|\0|\|! ) {
            return { status => ERROR(),
                     message => $app->translate("Invalid extra path '[_1]'", $relative_path)
                };
            }
            $path = File::Spec->catdir( $path, $relative_path );
            ## Untaint. We already checked for security holes in $relative_path.
            ($path) = $path =~ /(.+)/s;
            ## Build out the directory structure if it doesn't exist. DirUmask
            ## determines the permissions of the new directories.
            unless ( $fmgr->exists($path) ) {
                $fmgr->mkpath($path)
                  or return { status => ERROR(),
                              message => $app->translate("Can't make path '[_1]': [_2]",
                                            $path, $fmgr->errstr )
                            };
            }
        }
        $relative_url =
          File::Spec->catfile( $relative_path, encode_url($basename) );
        $relative_path = $relative_path
          ? File::Spec->catfile( $relative_path, $basename )
          : $basename;

        $asset_file = $q->param('site_path') ? '%r' : '%a';
        $asset_file = File::Spec->catfile( $asset_file, $relative_path );
        $local_file = File::Spec->catfile( $path, $basename );
        $base_url = $app->param('site_path') ? $blog->site_url
          : $blog->archive_url;
        $asset_base_url = $app->param('site_path') ? '%r' : '%a';

        ## Untaint. We have already tested $basename and $relative_path for security
        ## issues above, and we have to assume that we can trust the user's
        ## Local Archive Path setting. So we should be safe.
        ($local_file) = $local_file =~ /(.+)/s;

        ## If $local_file already exists, we try to write the upload to a
        ## tempfile, then ask for confirmation of the upload.
        if ( $fmgr->exists($local_file) ) {
            if ($has_overwrite) {
                my $tmp = $q->param('temp');
                if ( $tmp =~ m!([^/]+)$! ) {
                    $tmp = $1;
                }
                else {
                    return { status => ERROR(),
                 message => $app->translate( "Invalid temp file name '[_1]'", $tmp ) };
                }
                my $tmp_dir = $app->config('TempDir');
                my $tmp_file = File::Spec->catfile( $tmp_dir, $tmp );
                if ( $q->param('overwrite_yes') ) {
                    $fh = gensym();
                    open $fh, $tmp_file
                      or return { status => ERROR(),
                  message => $app->translate("Error opening '[_1]': [_2]",
                                 $tmp_file, "$!" )
                      };
                }
                else {
                    if ( -e $tmp_file ) {
                        unlink($tmp_file)
                          or return { status => ERROR(),
                      message => $app->translate("Error deleting '[_1]': [_2]",
                                 $tmp_file, "$!" )
                            };
                    }
            # TODO - not OK, fix, I think the best thing here is to return a status of "CANCELLED"
                    return start_upload($app);
                }
            }
            else {
                eval { require File::Temp };
                if ($@) {
                    return { status => ERROR(),
                 message => $app->translate(
                            "File with name '[_1]' already exists. (Install "
                              . "File::Temp if you'd like to be able to overwrite "
                              . "existing uploaded files.)",
                            $basename
                        )
                    };
                }

                my $tmp_dir = $app->config('TempDir');
                my ( $tmp_fh, $tmp_file );
                eval {
                    ( $tmp_fh, $tmp_file ) =
                      File::Temp::tempfile( DIR => $tmp_dir );
                };
                if ($@) {    #!$tmp_fh
                    return { status => ERROR(),
                 message => $app->errtrans(
                        "Error creating temporary file; please check your TempDir "
                          . "setting in your coniguration file (currently '[_1]') "
                          . "this location should be writable.",
                        (
                              $tmp_dir
                            ? $tmp_dir
                            : '[' . $app->translate('unassigned') . ']'
                        )
                    )};
                }
                require MT::CMS::Asset;
                defined( MT::CMS::Asset::_write_upload( $fh, $tmp_fh ) )
                  or return { status => ERROR(),
                  message => $app->translate(
                        "File with name '[_1]' already exists; Tried to write "
                          . "to tempfile, but open failed: [_2]",
                        $basename,
                        "$!"
                    )
                  };
                close $tmp_fh;
                my ( $vol, $path, $tmp ) = File::Spec->splitpath($tmp_file);
                my $base = File::Basename::basename($basename);
                return { status       => OVERWRITE(),
                         temp         => $tmp,
                         extra_path   => $relative_path_save,
                         site_path    => scalar $q->param('site_path'),
                         middle_path  => $middle_path,
                         fname        => $base,
                    };
            }
        }
    }
    else {
        $blog_id        = 0;
        $asset_base_url = '%s/support/uploads';
        $base_url       = $app->static_path . 'support/uploads';
        $param{support_path} =
          File::Spec->catdir( $app->static_file_path, 'support', 'uploads' );

        require MT::FileMgr;
        $fmgr = MT::FileMgr->new('Local');
        unless ( $fmgr->exists( $param{support_path} ) ) {
            $fmgr->mkpath( $param{support_path} );
            unless ( $fmgr->exists( $param{support_path} ) ) {
                return { status => ERROR(),
             message => $app->translate(
                    "Could not create upload path '[_1]': [_2]",
                        $param{support_path}, $fmgr->errstr
                ) };
            }
        }

        require File::Basename;
        my ($stem, undef, $type) = File::Basename::fileparse( $basename,
            qr/\.[A-Za-z0-9]+$/ );
        my $unique_stem = $stem;
        $local_file = File::Spec->catfile( $param{support_path},
            $unique_stem . $type );
        my $i = 1;
        while ($fmgr->exists($local_file)) {
            $unique_stem = join q{-}, $stem, $i++;
            $local_file = File::Spec->catfile( $param{support_path},
                $unique_stem . $type );
        }

        my $unique_basename = $unique_stem . $type;
        $relative_path  = $unique_basename;
        $relative_url   = encode_url($unique_basename);
        $asset_file     = File::Spec->catfile( '%s', 'support', 'uploads',
          $unique_basename );
    }

    require MT::Image;
    my ($w, $h, $id, $write_file) = MT::Image->check_upload(
        Fh => $fh, Fmgr => $fmgr, Local => $local_file,
        Max => $upload_param{max_size},
        MaxDim => $upload_param{max_image_dimension}
    );

    return { status => ERROR(),
             message => MT::Image->errstr }
        unless $write_file;

    ## File does not exist, or else we have confirmed that we can overwrite.
    my $umask = oct $app->config('UploadUmask');
    my $old   = umask($umask);
    defined( my $bytes = $write_file->() )
      or return { status => ERROR(),
                  message => $app->translate(
                            "Error writing upload to '[_1]': [_2]", $local_file,
                            $fmgr->errstr
              )
      };
    umask($old);

    ## Close up the filehandle.
    close $fh;

    ## If we are overwriting the file, that means we still have a temp file
    ## lying around. Delete it.
    if ( $q->param('overwrite_yes') ) {
        my $tmp = $q->param('temp');
        if ( $tmp =~ m!([^/]+)$! ) {
            $tmp = $1;
        }
        else {
            return { status => ERROR(),
             message => $app->translate( "Invalid temp file name '[_1]'", $tmp ) };
        }
        my $tmp_file = File::Spec->catfile( $app->config('TempDir'), $tmp );
        unlink($tmp_file)
        or return { status => ERROR(),
            message => $app->translate( "Error deleting '[_1]': [_2]", $tmp_file, "$!" ) };
    }

    ## We are going to use $relative_path as the filename and as the url passed
    ## in to the templates. So, we want to replace all of the '\' characters
    ## with '/' characters so that it won't look like backslashed characters.
    ## Also, get rid of a slash at the front, if present.
    $relative_path =~ s!\\!/!g;
    $relative_path =~ s!^/!!;
    $relative_url  =~ s!\\!/!g;
    $relative_url  =~ s!^/!!;
    my $url = $base_url;
    $url .= '/' unless $url =~ m!/$!;
    $url .= $relative_url;
    my $asset_url = $asset_base_url . '/' . $relative_url;

    require File::Basename;
    my $local_basename = File::Basename::basename($local_file);
    my $ext =
      ( File::Basename::fileparse( $local_file, qr/[A-Za-z0-9]+$/ ) )[2];

    require MT::Asset;
    my $asset_pkg = MT::Asset->handler_for_file($local_basename);
    my $is_image  = defined($w)
                 && defined($h)
                 && $asset_pkg->isa('MT::Asset::Image');
    my $asset =   $asset_pkg->load({ file_path => $asset_file,
                                     blog_id => $blog_id });
    if ( ! $asset ) {
        $asset = $asset_pkg->new();
        $asset->file_path($asset_file);
        $asset->file_name($local_basename);
        $asset->file_ext($ext);
        $asset->blog_id($blog_id);
        $asset->created_by( $app->user->id );
    }
    else {
        $asset->modified_by( $app->user->id );
    }
    my $original = $asset->clone;
    $asset->url($asset_url);
    if ($is_image) {
        $asset->image_width($w);
        $asset->image_height($h);
    }
    $asset->mime_type($mimetype) if $mimetype;
    $asset->save;
    $app->run_callbacks( 'cms_post_save.asset', $app, $asset, $original );

    if ($is_image) {
        $app->run_callbacks(
            'cms_upload_file.' . $asset->class,
            File  => $local_file,
            file  => $local_file,
            Url   => $url,
            url   => $url,
            Size  => $bytes,
            size  => $bytes,
            Asset => $asset,
            asset => $asset,
            Type  => 'image',
            type  => 'image',
            Blog  => $blog,
            blog  => $blog
        );
        $app->run_callbacks(
            'cms_upload_image',
            File       => $local_file,
            file       => $local_file,
            Url        => $url,
            url        => $url,
            Size       => $bytes,
            size       => $bytes,
            Asset      => $asset,
            asset      => $asset,
            Height     => $h,
            height     => $h,
            Width      => $w,
            width      => $w,
            Type       => 'image',
            type       => 'image',
            ImageType  => $id,
            image_type => $id,
            Blog       => $blog,
            blog       => $blog
        );
    }
    else {
        $app->run_callbacks(
            'cms_upload_file.' . $asset->class,
            File  => $local_file,
            file  => $local_file,
            Url   => $url,
            url   => $url,
            Size  => $bytes,
            size  => $bytes,
            Asset => $asset,
            asset => $asset,
            Type  => 'file',
            type  => 'file',
            Blog  => $blog,
            blog  => $blog
        );
    }

    return { status => SUCCESS(),
         asset => {
         url => $asset->url,
         id => $asset->id,
         width => $asset->image_width,
         height => $asset->image_height,
         },
         bytes => $bytes,
    };
}

1;
