#! /bin/sh

dist=""
vers=""

if [ -f "/etc/os-release" ]; then
    dist=$(cat /etc/os-release | sed -n "/^ID=/p" | head -1 |
               sed 's/^ID=//' |
               tr -d '"' | tr -d "'")
    vers=$(cat /etc/os-release | sed -n "/^VERSION_ID=/p" | head -1 |
               sed 's/^VERSION_ID=//' |
               tr -d '"' | tr -d "'")

    if [ -z "$vers" -a "$dist" = "debian" ]; then
        vers=unstable
    fi

    if [ -n "$vers" ]; then
        vers=":$vers"
    fi

    if [ "$dist" = "opensuse" -o "$dist" = "opensuse-leap" ]; then
        vers=$(echo "$vers" | sed 's/\..*//')
    fi
    
elif [ -f "/etc/centos-release" ]; then
    dist="centos"
    vers=$(cat /etc/centos-release | sed 's/^.* \([0-9]\)\.[0-9]* .*$/:\1/')

elif [ -f "/etc/redhat-release" ]; then
    dist="redhat"
    vers=$(cat /etc/centos-release | sed 's/^.* \([0-9]\)\.[0-9]* .*$/:\1/')

else
    dist="unknown"
fi

echo $dist$vers
