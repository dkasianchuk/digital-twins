import { useState, useRef } from 'react'
import './UploadModal.scss'

function UploadModal({isOpen, setIsOpen, setLang}) {
  const fileInputRef = useRef();
  const [files, setFiles] = useState([]);
  const isSubmit = !!files.length;

  const handleFileChange = (files) => {
    const newFiles = Array.from(files);
    setFiles((prevFiles) => [...prevFiles, ...newFiles]);

    if (fileInputRef.current) {
        fileInputRef.current.value = '';
    }
  };

  const handleFileRemove = (indexToRemove) => {
    setFiles((prevFiles) => 
        prevFiles.filter((_, index) => index !== indexToRemove));
  };

  const handleDrop = (e) => {
    e.preventDefault();
    e.stopPropagation();
    if (e.dataTransfer.files && e.dataTransfer.files.length > 0) {
      handleFileChange(e.dataTransfer.files);
      e.dataTransfer.clearData();
    }
  };

  const handleDrag = (e) => {
    e.preventDefault();
    e.stopPropagation();
  };


  const handleSubmit = async () => {

    const formData = new FormData();
    files.forEach((file) => {
      formData.append('files', file);
    });
    try {
      const response = await fetch('http://192.168.0.104:3000/v1/generate-parser', {
        method: 'POST',
        body: formData,
      });
      if (response.ok) {
        const data = await response.json();
        setFiles([]);
        setIsOpen(false);
        setLang(data.lang);
      } 
    } catch (error) {
      console.error('Upload error:', error);
    }
  };

  return (
    <>
      {isOpen && (
        <div className='modalWrapper'>
          <div className="modalBox" onDrop={handleDrop}
      onDragOver={(e) => handleDrag(e, true)}
      onDragLeave={(e) => handleDrag(e, false)}>
            <h3>Upload grammar files</h3>
            <div className='filesPlace' onClick={() => fileInputRef.current.click()}>
                📁 Choose Files
            </div>
            <input
              type="file"
              multiple
              ref={fileInputRef}
              onChange={(e) => handleFileChange(e.target.files)}
            />

            {files.length > 0 && (
              <ul>
                {files.map((file, idx) => (
                  <li key={idx}>
                    <span>{file.name}</span>
                    <div
                      className='removeIcon'
                      onClick={() => handleFileRemove(idx)}
                    >
                      ✖️
                    </div>
                  </li>
                ))}
              </ul>
            )}

            <div className="btnBox">
              {isSubmit && <button
                onClick={handleSubmit}
              >
                Submit
              </button>}
            </div>
          </div>
        </div>
      )}
    </>
  );
};

export default UploadModal
