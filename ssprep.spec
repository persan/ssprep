Summary: project template generator
Name: ssprep
Version: 1.5.7
Release: 1

License: GMGPL
Group:   Developmet/Tools
# URL: http://www.washington.edu/alpine
# FTP: ftp://ftp.cac.washington.edu/alpine/
# SVN: https://svn.cac.washington.edu/public/alpine/snapshots/
# Source0: ssprep-%{version}ftp://ftp.cac.washington.edu/alpine/alpine-%{version}.tar.bz2
Source0: ssprep-%{version}.tgz
BuildRoot: %(mktemp -ud %{_tmppath}/%{name}-%{version}-%{release}-XXXXXX)


%description
Alpine -- an Alternatively Licensed Program for Internet

%prep
%setup -q


%build
make compile


%install
rm -rf $RPM_BUILD_ROOT
make install PREFIX=$RPM_BUILD_ROOT

%clean
#rm -rf $RPM_BUILD_ROOT


%files
%defattr(-,root,root)
# %doc README
%{_bindir}/*


%changelog
* Wed Aug 27 2008 Rex Dieter <rdieter@fedoraproject.org> 2.00-1
- alpine-2.00 (#460332)


