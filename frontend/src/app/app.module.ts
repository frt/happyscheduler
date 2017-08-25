import { BrowserAnimationsModule } from '@angular/platform-browser/animations';
import { BrowserModule } from '@angular/platform-browser';
import { NgModule, Injectable } from '@angular/core';
import { HttpModule } from '@angular/http';
import { RouterModule } from '@angular/router';
import { ReactiveFormsModule } from '@angular/forms';
import { PushNotificationsModule, SimpleNotificationsModule, NotificationsService } from 'angular2-notifications';

import { AppComponent } from './app.component';
import { TaskListComponent } from './task-list/task-list.component';
import { NewTaskComponent } from './new-task/new-task.component';

@NgModule({
    declarations: [
        AppComponent,
        TaskListComponent,
        NewTaskComponent
    ],
    imports: [
        BrowserAnimationsModule,
        BrowserModule,
        ReactiveFormsModule,
        HttpModule,
        PushNotificationsModule,
        SimpleNotificationsModule.forRoot(),
        RouterModule.forRoot([
            {
                path: 'task-list',
                component: TaskListComponent
            },
            {
                path: '',
                redirectTo: '/task-list',
                pathMatch: 'full'
            }
        ])
    ],
    providers: [NotificationsService],
    bootstrap: [AppComponent]
})
export class AppModule { }
