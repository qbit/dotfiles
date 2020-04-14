function ifsource
	if test -f $argv
		source $argv
	end
end

ifsource /usr/local/share/taskwarrior/scripts/fish/task.fish
ifsource /usr/local/share/fish/functions/fzf-key-bindings.fish
ifsource ~/.po.fish

keychain --inherit any --agents ssh -q -Q
ifsource ~/.keychain/(uname -n)-fish

#dcolor

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

function mbuild
	set -l pkg (make show=PKGSTEM)
	make clean=all
	make && po -title "Port build complete!" \
		-body "$pkg build was successful!" || \
		po -title  "Port build failed" \
		-body "$pkg build failed!"
end

function install_go_tools
	set -l tools \
		github.com/jesseduffield/lazygit \
		github.com/jrick/ss \
		github.com/mdempsky/gocode \
		github.com/rjkroege/edwood \
		golang.org/x/lint/golint \
		golang.org/x/review/git-codereview \
		golang.org/x/tools/cmd/eg \
		golang.org/x/tools/cmd/fiximports \
		golang.org/x/tools/cmd/godoc \
		golang.org/x/tools/cmd/goimports \
		golang.org/x/tools/cmd/gorename \
		golang.org/x/tools/cmd/gorename \
		golang.org/x/tools/cmd/guru \
		golang.org/x/tools/cmd/present \
		golang.org/x/tools/cmd/stress \
		golang.org/x/tools/gopls \
		honnef.co/go/tools/cmd/staticcheck \
		mvdan.cc/gofumpt \
		rsc.io/goversion \
		suah.dev/ogvt

	for t in $tools;
		go get -v -u $t;
	end
end

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
