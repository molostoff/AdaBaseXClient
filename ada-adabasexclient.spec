#
# spec file for package ada-adabasexclient
#

%define pagname AdaBaseXClient
%define libname libadabasexclient
%define soname 0_2_1

Name:           ada-adabasexclient
Version:        0.2.1
Release:        1
Summary:        Ada client for BaseX
License:        MIT
Group:          Development/Libraries
URL:            https://github.com/ray2501/AdaBaseXClient
Source:         %{pagname}-%{version}.tar.gz
BuildRequires:  gcc-ada
BuildRequires:  make

%description
This package is Ada client for BaseX.
Works with BaseX 7.0 and later.


%package -n %{libname}%{soname}
Summary:        Library files for ada-adabasexclient
Group:          System/Libraries

%description -n %{libname}%{soname}
The %{libname}%{soname} package contains library files for ada-adabasexclient.


%package devel
Summary:        Development files for ada-adabasexclient
Requires:       %{name} = %{version}
Requires:       %{libname}%{soname} = %{version}

%description devel 
The %{name}-devel package contains source code and linking information for
developing applications that use ada-adabasexclient.

%prep
%setup -q -n %{pagname}-%{version}

%build
make PREFIX=/usr LIBDIR=lib64

%install
make DESTDIR=%{buildroot} PREFIX=/usr LIBDIR=lib64 install

%post -n %{libname}%{soname} -p /sbin/ldconfig
%postun -n %{libname}%{soname} -p /sbin/ldconfig

%files -n %{libname}%{soname}
%{_libdir}/*.so.*

%files
%license LICENSE
%doc README.md

%files devel
%{_includedir}/*
%{_libdir}/*.so
%{_libdir}/adabasexclient
%dir /usr/share/gpr
/usr/share/gpr/adabasexclient.gpr

%changelog

