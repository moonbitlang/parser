///|
fn join_path(dir : String, entry : String) -> String {
  if dir.has_suffix("/") {
    dir + entry
  } else {
    dir + "/" + entry
  }
}

///|
fn collect_mbt_files(dir : String, files : Array[String]) -> Unit raise Error {
  let entries = @fs.read_dir(dir) catch {
    err => fail("failed to read directory \{dir}: \{Repr(err)}")
  }
  for entry in entries {
    if entry != ".git" {
      let path = join_path(dir, entry)
      if !@fs.path_exists(path) {
        if entry.has_suffix(".mbt") {
          files.push(path)
        }
        continue
      }
      let is_dir = @fs.is_dir(path) catch {
        err => fail("failed to inspect \{path}: \{Repr(err)}")
      }
      if !is_dir && entry.has_suffix(".mbt") {
        files.push(path)
      } else if is_dir {
        collect_mbt_files(path, files)
      }
    }
  }
}

///|
fn reports_from_view(view : ArrayView[@basic.Report]) -> Array[@basic.Report] {
  let reports : Array[@basic.Report] = []
  for report in view {
    reports.push(report)
  }
  reports
}

///|
fn record_diagnostics(
  failures : Array[String],
  path : String,
  parser : String,
  reports : Array[@basic.Report],
) -> Unit {
  if !reports.is_empty() {
    let rendered = @basic.show_loc.protect(String, fn() {
      reports.to_json().stringify(indent=2)
    })
    failures.push("\{path}\nparser: \{parser}\ndiagnostics:\n\{rendered}")
  }
}

///|
fn impls_json(impls : @syntax.Impls) -> Json {
  @basic.show_loc.protect(String, fn() {
    impls.map(impl_ => impl_.json_repr()).to_json()
  })
}

///|
#skip("requires an external openseek checkout")
test "all OpenSeek MoonBit sources parse to equivalent ASTs without diagnostics" {
  let root = @env.get_env_var("OPENSEEK_ROOT").unwrap_or_else(() => {
    fail("OPENSEEK_ROOT must point to an OpenSeek checkout")
  })
  if root.is_empty() {
    fail("OPENSEEK_ROOT must not be empty")
  }
  let root_is_dir = @fs.is_dir(root) catch {
    err => fail("failed to inspect OPENSEEK_ROOT \{root}: \{Repr(err)}")
  }
  if !root_is_dir {
    fail("OPENSEEK_ROOT is not a directory: \{root}")
  }
  let files : Array[String] = []
  collect_mbt_files(root, files)
  files.sort()
  if files.is_empty() {
    fail("no .mbt files found under OPENSEEK_ROOT: \{root}")
  }
  let failures : Array[String] = []
  for path in files {
    let source = @fs.read_file_to_string(path) catch {
      err => fail("failed to read \{path}: \{Repr(err)}")
    }
    let (handrolled_impls, handrolled_reports) = @parser.parse_string(
      source,
      name=path,
      parser=Handrolled,
    )
    record_diagnostics(failures, path, "Handrolled", handrolled_reports)
    let (moonyacc_impls, moonyacc_reports) = @parser.parse_string(
      source,
      name=path,
      parser=MoonYacc,
    )
    record_diagnostics(failures, path, "MoonYacc", moonyacc_reports)
    let cst = @untyped_cst.parse_structure(source, name=path)
    record_diagnostics(
      failures,
      path,
      "untyped CST",
      reports_from_view(cst.diagnostics_view()),
    )
    let (cst_impls, lowering_reports) = cst.to_impls()
    record_diagnostics(failures, path, "untyped CST to_impls", lowering_reports)
    if !(handrolled_reports.is_empty() &&
      moonyacc_reports.is_empty() &&
      cst.diagnostics_view().is_empty() &&
      lowering_reports.is_empty()) {
      continue
    }
    let handrolled_json = impls_json(handrolled_impls)
    let moonyacc_json = impls_json(moonyacc_impls)
    let cst_json = impls_json(cst_impls)
    if handrolled_json != moonyacc_json {
      failures.push(
        "\{path}\nAST mismatch: Handrolled vs MoonYacc\nHandrolled:\n\{handrolled_json.stringify(indent=2)}\nMoonYacc:\n\{moonyacc_json.stringify(indent=2)}",
      )
    }
    if handrolled_json != cst_json {
      failures.push(
        "\{path}\nAST mismatch: Handrolled vs untyped CST\nHandrolled:\n\{handrolled_json.stringify(indent=2)}\nuntyped CST:\n\{cst_json.stringify(indent=2)}",
      )
    }
  }
  if !failures.is_empty() {
    fail(
      "OpenSeek corpus validation failed for \{failures.length()} checks:\n\n" +
      failures.join("\n\n"),
    )
  }
  println(
    "parsed \{files.length()} OpenSeek .mbt files with equivalent ASTs from all three parsers",
  )
}
