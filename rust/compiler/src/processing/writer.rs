use std::path::{Path, PathBuf};

pub struct TreeWriter {
    pub root_dir: PathBuf,
}

impl TreeWriter {
    pub fn new(root_dir: PathBuf) -> Self {
        TreeWriter { root_dir }
    }

    /// Write a file to the specifed path, creating parent directories as required
    pub fn write(&self, path: &Path, content: String) -> Result<(), std::io::Error> {
        let mut file_path = self.root_dir.clone();
        file_path.push(path);
        log::info!("writing {}", file_path.display());
        let dir_path = file_path.parent().unwrap();
        std::fs::create_dir_all(dir_path)?;
        std::fs::write(file_path, &content)?;
        Ok(())
    }
}
