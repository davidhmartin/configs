# Project Memory: Emacs Configuration

## Project Context
This is a configuration repository focused on Emacs setup and customization. The primary goal is developing and maintaining Emacs configuration files.

## File Structure
- `emacs/init.el` - Main Emacs configuration file

## Emacs Configuration Preferences

### Configuration Style
- Use straightforward Elisp in init.el
- Configure `use-package` to use the `straight` package manager
- Focus on modern Emacs practices and packages
- Prioritize LSP-based development workflows

### Key Areas of Focus
- LSP integration and language server setup
- Modern completion frameworks (company, corfu, etc.)
- Project management and navigation
- Version control integration
- Code formatting and linting
- Efficient keybindings and workflows

### Development Approach
- Test changes incrementally
- Maintain compatibility with recent Emacs versions
- Document configuration choices clearly
- Keep performance considerations in mind

## Instructions for Claude
- Always examine existing configuration before suggesting changes
- Follow established patterns in the current config
- Explain the purpose and benefits of suggested packages or settings
- Prefer well-maintained, actively developed packages
- Consider startup time impact when adding new packages