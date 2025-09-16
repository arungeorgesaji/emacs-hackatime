# emacs-hackatime

Emacs integration for HackaTime productivity tracking.

## Installation

1. **Clone the repository:**
   ```bash
   git clone https://github.com/arungeorgesaji/emacs-hackatime.git 
   mv emacs-hackatime/utils.el ~/.emacs.d/lisp/
   mv emacs-hackatime/client.el ~/.emacs.d/lisp/
   ```

2. **Add to your `init.el`:**
   ```elisp
   (setq hackatime-api-key "your-api-key-here")
   (add-to-list 'load-path "~/.emacs.d/lisp/")
   (require 'client)
   ```

3. **Set your API key** in the `hackatime-api-key` variable

## Optional Debug Mode

Add this to see debug messages for troubleshooting:
```elisp
(setq hackatime-debug-mode t)
```

## Files Structure

```
.emacs.d/
├── init.el
└── lisp/
    ├── client.el
    └── utils.el
```

The package will automatically start tracking time when Emacs loads with a valid API key.
