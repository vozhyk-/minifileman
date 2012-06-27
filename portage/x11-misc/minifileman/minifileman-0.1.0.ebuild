# Copyright 1999-2012 Gentoo Foundation
# Distributed under the terms of the GNU General Public License v2
# $Header: $

inherit common-lisp-2 eutils

if [[ ${PV} == 9999 ]]; then
    EDARCS_REPOSITORY="http://patch-tag.com/r/vozhyk_/minifileman"
    inherit darcs
fi

DESCRIPTION="minifileman is a light and extensible file manager."
HOMEPAGE="http://sourceforge.net/projects/minifileman"
SRC_URI="http://downloads.sourceforge.net/project/minifileman/lisp/${P}.tar.gz"
        # (nonexistant)

[[ ${PV} == 9999 ]] &&
    SRC_URI=""

LICENSE="BSD-2"
SLOT="0"
KEYWORDS="~x86 ~amd64"
IUSE=""

DEPEND=""
RDEPEND="dev-lisp/cl-ltk
         dev-lisp/cl-fad
         dev-lisp/closer-mop
         >dev-lisp/metabang-bind-0.7.4
         dev-lisp/iterate
         dev-lisp/alexandria
         dev-lisp/kmrcl"

CLSYSTEMS="minifileman"


if [[ ${PV} == 9999 ]]; then
    src_unpack() {
        darcs_src_unpack
    }
fi

src_install() {
    cd src/
    common-lisp-install *.{lisp,asd}
    common-lisp-symlink-asdf
}
