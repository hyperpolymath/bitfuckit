# SPDX-License-Identifier: AGPL-3.0-or-later
# Nickel configuration for bitfuckit

{
  project = {
    name = "bitfuckit",
    version = "0.1.0",
    language = "ada",
    license = "AGPL-3.0-or-later",
    description = "Bitbucket CLI tool written in Ada",
  },

  build = {
    system = "gprbuild",
    project_file = "bitfuckit.gpr",
    output_dir = "bin",
    binary = "bitfuckit",

    commands = {
      build = "gprbuild -P bitfuckit.gpr",
      clean = "gprclean -P bitfuckit.gpr",
      release = "gprbuild -P bitfuckit.gpr -XBUILD_MODE=release",
      verify = "gnatprove -P bitfuckit.gpr --mode=check",
    },
  },

  install = {
    user_bin = "~/.local/bin",
    system_bin = "/usr/local/bin",
    man_dir = "/usr/share/man/man1",
    completion_dirs = {
      bash = "/usr/share/bash-completion/completions",
      zsh = "/usr/share/zsh/site-functions",
      fish = "/usr/share/fish/vendor_completions.d",
    },
  },

  dependencies = {
    build = [
      { name = "gnat", version = ">= 12" },
      { name = "gprbuild", version = ">= 22" },
    ],
    runtime = [
      { name = "curl", version = ">= 7" },
    ],
  },

  platforms = {
    linux = { supported = true, tested = true },
    macos = { supported = true, tested = false },
    windows = { supported = false, tested = false },
  },
}
