# -*- mode: ruby -*-
# vi: set ft=ruby :

# FreeBSD vagrant for builds
Vagrant.configure("2") do |config|
  config.vm.box = "freebsd/FreeBSD-12.1-STABLE"
  config.vm.synced_folder ".", "/home/vagrant/libarchive", type: "rsync", rsync__exclude: [".git/", "dist-newstyle"]
  config.vm.provider "virtualbox" do |v|
    v.memory = 2048
  end
  config.vm.provision "shell",
    inline: "pkg install --yes hs-cabal-install ghc wget curl && cabal update && export PATH=$HOME/.cabal/bin:$PATH"
end
