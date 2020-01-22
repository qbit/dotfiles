source /usr/local/share/fish/functions/fzf-key-bindings.fish
source ~/.keychain/(uname -n)-fish

fzf_key_bindings

set -Ux fish_greeting ""

alias goget='env GO111MODULES=on go get'
alias ag='ag --nocolor'
alias build="dpb -c -h /home/buildie/hosts -P"
alias cabal='env TMPDIR=/usr/local/cabal/build/ cabal'
alias mkae='make'
alias mutt='stty discard undef; neomutt'
alias pass="gopass"
alias pup='doas -n /usr/sbin/pkg_add -u'
alias rustc='rustc --color=never'
alias sbcl="rlwrap sbcl"
alias tmux="tmux -u2"
alias vi='vim'

function pclean
	find . -name \*.orig -exec rm {} \;
	find . -size 0 -exec rm {} \;
end

function src
	cd /usr/src/*/$argv || return
end

function port
	cd /usr/ports/*/$argv 2>/dev/null || \
		cd /usr/ports/*/*/$argv 2>/dev/null || \
		return
end

function k
	set K "$HOME/.k"
	if test -z $argv
		echo $PWD >> $K
	else
		switch $argv
			case 'clean'
				sort $K | uniq > {$K}.tmp && mv {$K}.tmp {$K}
			case 'rm'
				sed -i -E "\#^$PWD\$#d" $K
			case 'ls'
				cat $K
			case '*'
				set d (grep -e "$argv" {$K} | head -n 1)
				cd $d
		end
	end
end
