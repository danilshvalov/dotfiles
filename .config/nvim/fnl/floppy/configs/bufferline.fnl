(local bufline (require :bufferline))
(local {:cycle bufcycle :move bufmove : go_to_buffer} bufline)
(local {: bufdelete} (require :bufdelete))
(local {: nmap} (require :floppy.utils))
(local api vim.api)

(fn close-buffer [id]
  (local (res _) (pcall vim.cmd (.. :bd (tostring id))))
  (if (not res)
      (print "Cannot close buffer:" (api.nvim_buf_get_name id))))

(fn is-valid-buf [buf]
  (and (api.nvim_buf_is_valid buf) (. vim.bo buf :buflisted)))

(fn valid-buffers []
  (vim.tbl_filter is-valid-buf (api.nvim_list_bufs)))

(fn close-others []
  (local bufnr (api.nvim_get_current_buf))
  (local buffers (valid-buffers))
  (if (> (length buffers) 1)
      (each [_ id (pairs buffers)]
        (if (not= id bufnr)
            (close-buffer id)))))

(fn setup-mappings []
  (nmap :<Leader>q (partial bufdelete 0 false))
  (nmap :<C-j> (partial bufcycle -1))
  (nmap :<C-k> (partial bufcycle 1))
  (nmap :<C-A-j> (partial bufmove -1))
  (nmap :<C-A-k> (partial bufmove 1))
  (nmap :<Leader>co (partial close-others))
  (for [n 1 9]
    (nmap (.. :<Leader> (tostring n)) (partial go_to_buffer n))))

(fn setup-bufferline []
  (bufline.setup {:options {:show_buffer_icons false
                            :show_close_icon false
                            :diagnostics :nvim_lsp
                            :diagnostics_update_in_insert true
                            :diagnostics_indicator (fn [count _ _ _]
                                                     (tostring count))
                            :numbers (fn [opts]
                                       opts.ordinal)
                            :tab_size 0}}))

(fn setup []
  (setup-bufferline)
  (setup-mappings))

{: setup}
