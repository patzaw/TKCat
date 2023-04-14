function(data, type, row, meta) {
   function formatFileSize(fileSizeInBytes) {
     const units = ['B', 'KB', 'MB', 'GB', 'TB', "PB", "EB"];
     let size = fileSizeInBytes;
     let unitIndex = 0;
     while (size >= 1024 && unitIndex < units.length - 1) {
       size /= 1024;
       unitIndex++;
     }
     return `${size.toFixed(1)} ${units[unitIndex]}`;
   }
   // Call the formatFileSize function to format the file size for display
   // data contains the value of the 'filesize' property in the row data
   // Use meta object to access the current cell and store the original
   // value as a data attribute
   const formattedSize = formatFileSize(data);
   if (type === 'display') {
      $(meta.cell).attr('data-sort', data); // Store original value as data attribute
      return formattedSize;
   } else {
      return data; // Return original value for sorting/filtering
   }
}
