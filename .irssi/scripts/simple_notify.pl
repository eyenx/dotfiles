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
    authors     => 'gnomeye',
    contact     => 'gnomeye@gmail.com',
    name        => 'simple_notifier.pl',
    description => 'use simple tmp file to notify',
    license     => 'GNU General Public License',
);

Irssi::settings_add_str('simple_notify','notify_file', '/tmp/.notify_irssi');

sub wf{
	my $str = shift;
	open FL, '>', Irssi::settings_get_str('notify_file') or die;
	print FL $str;
	close FL;
}
sub unfl {
	my ($d,$o) = @_;
	wf('0')	if ($o == 3 && $d->{data_level} < $o);
}
sub unflsend {
    my ($x,$y,$z)=@_;
	wf('0') if $z; 
}
sub unflman {
    wf('0');
}
sub flag {
	wf('1');
}
sub ser {
	my ($d) = @_;
	my $s=$d->{server};
	return if (!$s||!($d->{level} & MSGLEVEL_HILIGHT));
	flag();
}
wf(0);
Irssi::signal_add('print text', 'ser');
Irssi::signal_add('message private', 'flag');
Irssi::signal_add('dcc request', 'flag');
Irssi::signal_add('window activity', 'unfl');
Irssi::signal_add('send command', 'unflsend');

Irssi::command_bind('notify_unflag','unflman');
