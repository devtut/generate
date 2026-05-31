---
title: "ThreadPoolTaskExecutor: configuration and usage"
description: "application configuration"
---

## application configuration


```java
@Configuration
@EnableAsync
public class ApplicationConfiguration{
    
    @Bean
    public TaskExecutor getAsyncExecutor() {
        ThreadPoolTaskExecutor executor = new ThreadPoolTaskExecutor();
        executor.setCorePoolSize(2);
        executor.setThreadNamePrefix("executor-task-");
        executor.initialize();
        return executor;
    }
    
}

```