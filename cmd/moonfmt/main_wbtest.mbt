///|
fn register_temp_dir_cleanup(
  group : @async.TaskGroup[Unit],
  directory : String,
) -> Unit {
  group.add_defer() <| () => {
    @async.protect_from_cancel(() => @fs.rmdir(directory, recursive=true))
  }
}

///|
async test "unchanged overwrite leaves file unchanged" {
  @async.with_task_group() <| group => {
    let directory = @fs.tmpdir(prefix="moonfmt-unchanged")
    register_temp_dir_cleanup(group, directory)
    let target = directory + "/input.mbt"
    @fs.write_file(target, "same", create_mode=CreateNew)
    let changed = overwrite_if_changed(target, "same", "same")
    assert_false(changed)
    inspect(@fs.read_file(target).text(), content="same")
  }
}

///|
async test "changed overwrite writes new contents" {
  @async.with_task_group() <| group => {
    let directory = @fs.tmpdir(prefix="moonfmt-changed")
    register_temp_dir_cleanup(group, directory)
    let target = directory + "/input.mbt"
    @fs.write_file(target, "old", create_mode=CreateNew)
    let changed = overwrite_if_changed(target, "old", "new")
    assert_true(changed)
    inspect(@fs.read_file(target).text(), content="new")
  }
}

///|
#cfg(all(target="native", not(platform="windows")))
async test "overwrite preserves executable permission" {
  @async.with_task_group() <| group => {
    let directory = @fs.tmpdir(prefix="moonfmt-permission")
    register_temp_dir_cleanup(group, directory)
    let target = directory + "/input.mbt"
    @fs.write_file(target, "old", create_mode=CreateNew)
    @fs.chmod(target, 0o755)
    assert_true(@fs.can_execute(target))
    let changed = overwrite_if_changed(target, "old", "new")
    assert_true(changed)
    assert_true(@fs.can_execute(target))
    inspect(@fs.read_file(target).text(), content="new")
  }
}

///|
#cfg(all(target="native", not(platform="windows")))
async test "overwrite follows symbolic link without replacing it" {
  @async.with_task_group() <| group => {
    let directory = @fs.tmpdir(prefix="moonfmt-symlink")
    register_temp_dir_cleanup(group, directory)
    let target = directory + "/target.mbt"
    let link = directory + "/input.mbt"
    @fs.write_file(target, "old", create_mode=CreateNew)
    @fs.symlink(link, target~)
    let changed = overwrite_if_changed(link, "old", "new")
    assert_true(changed)
    assert_true(@fs.kind(link, follow_symlink=false) == SymLink)
    inspect(@fs.read_file(target).text(), content="new")
  }
}
