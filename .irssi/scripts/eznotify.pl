##
## Put me in ~/.irssi/scripts, and then execute the following in irssi:
##
##       /load perl
##       /script load simple_notify.pl
##
## puts a 1 into the tmp file when a new message,query has been received. If it's not in the active window, a simple switch to the highlighted window will change the status into the file back to 0. If it is the active window, sending a message will set it back to 0 (send command signal).

use strict;
use Irssi;
use vars qw($VERSION %IRSSI);

$VERSION = "0.1";
%IRSSI = (
    authors     => 'eye',
    contact     => 'eye@eyenx.ch',
    name        => 'eznotify.pl',
    description => 'do something on msg or highlight (put some str in file)',
    license     => 'GNU General Public License',
);

Irssi::settings_add_str('eznotify','notify_file', '/tmp/.notifyirssi');

sub flag{
        my $str = "1";
        open FL,'>', Irssi::settings_get_str('notify_file') or die;
        print FL $str; 
        close FL;
}
sub hig {
	my ($d) = @_;
	my $s=$d->{server};
	return if (!$s||!($d->{level} & MSGLEVEL_HILIGHT));
	flag();
}

Irssi::signal_add('print text', 'hig');
Irssi::signal_add('message private', 'flag');
Irssi::signal_add('dcc request', 'flag');

