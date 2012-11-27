/* Jungo Confidential. Copyright (c) 2011 Jungo Ltd.  http://www.jungo.com */

/* 
 * Header file for WinDriver Linux.
 *
 * This file may be distributed only as part of the 
 * application you are distributing, and only if it 
 * significantly contributes to the functionality of your 
 * application. (see \windriver\docs\license.txt for details).
 *
 * Web site: http://www.jungo.com
 * Email:    support@jungo.com
 */

#ifndef _LINUX_WRAPPERS_H_
#define _LINUX_WRAPPERS_H_

#if defined(__cplusplus)
    extern "C" {
#endif

#include <stdarg.h>
#define ATOMIC 0x1
#define DMA    0x2
struct semaphore;
struct pt_regs;
struct inode;
struct file;
struct timer_list;

typedef unsigned long long _u64;

typedef struct
{
    _u64 phys; // Physical address of page.
    unsigned long size; // Size of page.
} LINUX_dma_page;     

typedef struct
{
    unsigned int bus_num;
    unsigned int devfn;
    int irq;
    unsigned short vid;
    unsigned short did;
    unsigned char hdr_type;
    void *dev_h;
} LINUX_pnp_data;

#if !defined(__WD_LINUX_SYNC_TYPES__)
#define __WD_LINUX_SYNC_TYPES__
    typedef struct os_spinlock
    {
        void *spinlock;
        unsigned long flags;
    } os_spinlock_t;
    typedef volatile int os_interlocked_t;
#endif

typedef unsigned long (* kp_register_mod_func_t)(void *, void *);

#define LINUX_DMA_FROM_DEVICE 2
#define LINUX_DMA_TO_DEVICE 1
#define LINUX_DMA_TO_FROM_DEVICE 0

int LINUX_down_interruptible(struct semaphore * sem);
void LINUX_up(struct semaphore * sem);
struct semaphore * LINUX_create_mutex(void);
void LINUX_free_mutex(struct semaphore * sem);
void * LINUX_vmalloc(unsigned long size);
void LINUX_vfree(void * addr);
void *LINUX_kmalloc(unsigned int size, int flag);
void *LINUX_dma_contig_alloc(void *pdev, unsigned int size, int flag,
    _u64 *phys);
void LINUX_dma_contig_free(void *pdev, void *va, unsigned int size, _u64 phys);
void LINUX_kfree(const void *addr);
unsigned long LINUX_copy_from_user(void *to, const void *from, unsigned long n);
unsigned long LINUX_copy_to_user(void *to, const void *from, unsigned long n);
void LINUX_MOD_INC_USE_COUNT(void);
void LINUX_MOD_DEC_USE_COUNT(void);
int LINUX_register_chrdev(unsigned int major, const char *name);
void LINUX_unregister_chrdev(unsigned int major, const char *name);
const char *LINUX_get_driver_name(void);
kp_register_mod_func_t LINUX_get_register_kp_module_func(void);
void LINUX_init_register_kp_module_func(void);
void LINUX_kp_inc_ref_count(void);
void LINUX_kp_dec_ref_count(void);
int LINUX_register_ioctl32_conversion(unsigned int cmd);
int LINUX_unregister_ioctl32_conversion(unsigned int cmd);
void LINUX_queue_task_bh_immediate(void *bh);
void LINUX_schedule_task(void *bh);
void LINUX_bh_free(void *bh);
void *LINUX_bh_alloc(void (*routine)(void *), void *data);
int LINUX_request_irq(unsigned int irq, int is_shared, const char *device,
    void *ctx);
void LINUX_free_irq(unsigned int irq, void *dev_id);
int LINUX_get_irq(void *pdev);
int LINUX_pci_enable_msi(void *pdev);
void LINUX_pci_disable_msi(void *pdev);
int LINUX_request_irq_msix(void *pdev, int is_shared, const char *device,
    void *ctx);
void LINUX_free_irq_msix(void *pdev, void *ctx);
int LINUX_pci_enable_msix(void *pdev, int num_entries);
void LINUX_pci_disable_msix(void *pdev);
unsigned long LINUX_ioremap(unsigned long offset, unsigned long size);
void LINUX_iounmap(unsigned long addr);
unsigned long LINUX_do_mmap(struct file *file, 
    unsigned long len, unsigned long phys_addr, void *kernel_addr);
int LINUX_do_munmap(unsigned long addr, unsigned int len);
void LINUX_cli(void);
void LINUX_sti(void);
int LINUX_pcibios_present(void);
int LINUX_pcibios_read_config_byte (unsigned char bus, unsigned char dev_fn,
    unsigned int where, unsigned char *val);
int LINUX_pcibios_read_config_word (unsigned char bus, unsigned char dev_fn,
    unsigned int where, unsigned short *val);
int LINUX_pcibios_read_config_dword (unsigned char bus, unsigned char dev_fn,
    unsigned int where, unsigned int *val);
int LINUX_pcibios_write_config_byte (unsigned char bus, unsigned char dev_fn,
    unsigned int where, unsigned char val);
int LINUX_pcibios_write_config_word (unsigned char bus, unsigned char dev_fn,
    unsigned int where, unsigned short val);
int LINUX_pcibios_write_config_dword (unsigned char bus, unsigned char dev_fn,
    unsigned int where, unsigned int val);
int LINUX_mmconfig_enabled(void);
void LINUX_udelay(unsigned long usecs);
void LINUX_schedule(void);
long LINUX_schedule_timeout(long timeout);

void LINUX_register_windriver_symboles(void);
void LINUX_register_symboles(void);
int WDlinuxOpen(struct inode *inode, struct file *filp);
int WDlinuxClose(struct inode *inode, struct file *filp);
int WDlinuxIoctl(struct inode *inode, struct file *filp, unsigned int cmd, unsigned long arg);
int wd_linux_ioctl_compat(struct inode *inode, struct file *filp, unsigned int cmd, unsigned long arg);
int init_module_cpp(void);
void cleanup_module_cpp(void);
unsigned int LINUX_need_copy_from_user(void);
struct semaphore * LINUX_create_semaphore(void);
void LINUX_free_semaphore(struct semaphore * sem);
unsigned long LINUX_jiffies(void);
long LINUX_get_time_in_sec(void);
int LINUX_printk(const char *fmt, ...)
        __attribute__ ((format (printf, 1, 2)));
int LINUX_sprintf(char *buf, const char *fmt, ...)
        __attribute__ ((format (printf, 2, 3)));
int LINUX_snprintf(char *buf, unsigned long n, const char *fmt, ...)
        __attribute__ ((format (printf, 3, 4)));
int LINUX_vsprintf(char *buf, const char *fmt, va_list args);
int LINUX_vsnprintf(char *buf, unsigned long n, const char *fmt, va_list args);
unsigned long LINUX_virt_to_phys(void *va);
_u64 LINUX_pci_map_single(void *dev, void *va, unsigned long size,
    unsigned int dma_direction);
void LINUX_pci_unmap_single(void *dev, _u64 pa, unsigned long size,
    unsigned int dma_direction);
void LINUX_mem_map_reserve(void *addr, unsigned long size);
void LINUX_mem_map_unreserve(void *addr, unsigned long size);
unsigned int LINUX_is_version_2_0(void);
unsigned long LINUX_usecs_to_jiffies(unsigned long usecs);
unsigned long LINUX_msecs_to_jiffies(unsigned long msecs);
void LINUX_add_timer(struct timer_list *timer, unsigned long timeout_msecs);
void LINUX_create_timer(struct timer_list **timer, 
    void ( * timer_cb)(unsigned long), unsigned long ctx);
void LINUX_del_timer(struct timer_list *timer);
void LINUX_destroy_timer(struct timer_list *timer);
int LINUX_user_page_list_get(void *buf, unsigned long bytes, 
    void **pl_h);
void LINUX_user_page_list_put(void *pl_h);
void LINUX_page_list_lock(void *pl_h);
void LINUX_page_list_unlock(void *pl);
int LINUX_page_list_copyin(void *pl_h, unsigned long offset, const void *src, 
    unsigned long bytes);
int LINUX_page_list_copyout(void *pl_h, unsigned long offset,
    void *dst, unsigned long bytes);
int LINUX_build_sg_dma(LINUX_dma_page *page_list, unsigned int *dma_sglen, 
    void *buf, unsigned long size, unsigned int dma_direction, void *dev_handle, 
    void **dma_handle);
int LINUX_free_sg_dma(void *dma_handle, void *buf, unsigned long size,
    unsigned int dma_direction, void *dev_handle);
void LINUX_pci_dma_sync_single_for_cpu(void *dev, _u64 dma_addr,
    unsigned long size, unsigned int dma_direction);
void LINUX_pci_dma_sync_single_for_device(void *dev, _u64 dma_addr,
    unsigned long size, unsigned int dma_direction);
void LINUX_pci_dma_sync_sg_for_cpu(void *dev, void *dma_handle, int nelems,
    unsigned int dma_direction);
void LINUX_pci_dma_sync_sg_for_device(void *dev, void *dma_handle, int nelems,
    unsigned int dma_direction);
int LINUX_pci_set_dma_mask(void *dev, _u64 dma_mask);
unsigned int LINUX_get_version(void);
void LINUX_spin_lock_init(os_spinlock_t *lock);
void LINUX_spin_lock_uninit(os_spinlock_t *lock);
void LINUX_spin_unlock_irq(os_spinlock_t *lock);
void LINUX_spin_lock_irq(os_spinlock_t *lock);
void LINUX_spin_unlock_irqrestore(os_spinlock_t *lock);
void LINUX_spin_lock_irqsave(os_spinlock_t *lock);
int LINUX_atomic_inc(os_interlocked_t *val);
int LINUX_atomic_dec(os_interlocked_t *val);
int LINUX_atomic_add(os_interlocked_t *val, int i);
int LINUX_atomic_read(os_interlocked_t *val);
void LINUX_atomic_set(os_interlocked_t *val, int i);
int LINUX_atomic_xchg(os_interlocked_t *val, int i);
int LINUX_atomic_cmpxchg(os_interlocked_t *val, int cmp, int i);
void LINUX_atomic_init(os_interlocked_t *val);
void LINUX_atomic_uninit(os_interlocked_t *val);
void LINUX_event_create(struct semaphore **event);
void LINUX_event_wait(struct semaphore **event);
void LINUX_event_set(struct semaphore **event);
void LINUX_event_destroy(struct semaphore **event);
void LINUX_flush_scheduled_tasks(void);

void LINUX_pci_set_master(void *dev_h);
int LINUX_pci_enable_device(void *dev_h);
void LINUX_pci_disable_device(void *dev_h);
int LINUX_pci_request_regions(void *dev_h, char *modulename);
void LINUX_pci_release_regions(void *dev_h);

unsigned char LINUX_inb(unsigned short port);
unsigned short LINUX_inw(unsigned short port);
unsigned int LINUX_inl(unsigned short port);
void LINUX_outb(unsigned char value, unsigned short port);
void LINUX_outw(unsigned short value, unsigned short port);
void LINUX_outl(unsigned int value, unsigned short port);
void LINUX_insb(unsigned short port, void *addr, unsigned long count);
void LINUX_insw(unsigned short port, void *addr, unsigned long count);
void LINUX_insl(unsigned short port, void *addr, unsigned long count);
void LINUX_insw(unsigned short port, void *addr, unsigned long count);
void LINUX_outsb(unsigned short port, void *addr, unsigned long count);
void LINUX_outsw(unsigned short port, void *addr, unsigned long count);
void LINUX_outsl(unsigned short port, void *addr, unsigned long count);
char *LINUX_strcpy(char *dest,const char *src);
void *LINUX_memset(void *adrr ,int s, unsigned long count);
int LINUX_strncmp(const char *cs,const char *ct, unsigned long count);
int LINUX_strcmp(const char *cs,const char *ct);
char *LINUX_strcat(char *dest,const char *src);
char *LINUX_strncat(char *dest,const char *src,unsigned long count);
void *LINUX_memcpy(void *to, const void *from, unsigned long n);
int LINUX_memcmp(void *to, const void *from, unsigned long n);
unsigned long LINUX_strlen(const char *s);
char *LINUX_strncpy(char *dest, const char *src, unsigned long count);
unsigned long LINUX_pci_resource_len(void *dev_h, int i);
unsigned long LINUX_pci_resource_start(void *dev_h, int i);
int LINUX_pci_resource_type(void *dev_h, int i);
void LINUX_pci_get_pnp_data(void *dev_h, LINUX_pnp_data *p);
void LINUX_pci_set_irq(void *dev_h, unsigned char irq);
void *LINUX_pci_find_slot(unsigned int bus, unsigned int devfn);
void *LINUX_pci_bus(void *dev_h);
void *LINUX_pci_bus_subordinate(void *dev_h);
void *LINUX_pci_find_bus(int domain, int busnr);
int LINUX_pci_scan_slot(void *bus_h, int devfn);
void LINUX_pci_bus_add_devices(void *bus_h);
void LINUX_pci_bus_size_bridges(void *bus_h);
void LINUX_pci_bus_assign_resources(void *bus_h);
void LINUX_pci_remove_bus_device(void *dev_h);
unsigned long LINUX_get_page_size(void);
unsigned long LINUX_get_page_shift(void);
void LINUX_write8(unsigned char val, volatile void *addr);
void LINUX_write16(unsigned short val, volatile void *addr);
void LINUX_write32(unsigned int val, volatile void *addr);
void LINUX_write64(_u64 val, volatile void *addr);
unsigned char LINUX_read8(volatile void *addr);
unsigned short LINUX_read16(volatile void *addr);
unsigned int LINUX_read32(volatile void *addr);
_u64 LINUX_read64(volatile void *addr);

#if defined(__cplusplus)
    }
#endif
    
#endif //_LINUX_WRAPPERS_H_
